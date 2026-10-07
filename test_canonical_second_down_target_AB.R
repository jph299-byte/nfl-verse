# NFL NUMBERS — FINAL CANONICAL SECOND-DOWN TARGET A/B TEST
#
# This wrapper runs canonical_situation_v3.R twice.
# The ONLY scoring-rule difference is the 2nd-down target:
#   CURRENT:  60% of yards to go
#   PROPOSED: 100% on 2nd & 1-4; 70% on 2nd & 5+
#
# Everything else is inherited directly from canonical_situation_v3.R.

canonical_file <- "canonical_situation_v3.R"
if (!file.exists(canonical_file)) stop("Cannot find ", canonical_file, " in repository root.")

src <- readLines(canonical_file, warn=FALSE)

old_target <- "  target <- if (d==1) .4*togo else if (d==2) .6*togo else togo"
new_target <- paste0(
  "  target <- if (d==1) .4*togo else if (d==2) ",
  "if (togo<=4) togo else .7*togo else togo"
)

if (sum(src == old_target) != 1) {
  stop("Safety check failed: expected exactly one canonical target line, found ",
       sum(src == old_target), ". No test run.")
}

make_variant <- function(name, proposed=FALSE) {
  x <- src
  if (proposed) x[x == old_target] <- new_target

  # Give each run its own output directory.
  out_old <- 'OUT <- "situation_v3_full"'
  out_new <- paste0('OUT <- "canonical_second_down_AB/', name, '"')
  if (sum(x == out_old) != 1) {
    stop("Safety check failed: could not uniquely replace canonical OUT line.")
  }
  x[x == out_old] <- out_new

  f <- paste0("canonical_second_down_", name, ".R")
  writeLines(x, f)
  f
}

dir.create("canonical_second_down_AB", showWarnings=FALSE)

current_file  <- make_variant("current_60", FALSE)
proposed_file <- make_variant("proposed_100_70", TRUE)

run_variant <- function(f) {
  cat("\nRUNNING ", f, "\n", sep="")
  status <- system2("Rscript", c(f, "4"))
  if (status != 0) stop("Canonical variant failed: ", f)
}

run_variant(current_file)
run_variant(proposed_file)

readv <- function(v, file) {
  read.csv(file.path("canonical_second_down_AB", v, file),
           stringsAsFactors=FALSE)
}

variants <- c("current_60", "proposed_100_70")

# ---- Calibration / odd-even validation ----
folds <- do.call(rbind, lapply(variants, function(v) {
  x <- readv(v, "situation_v3_odd_even_validation.csv")
  x$variant <- v
  x
}))

fold_summary <- aggregate(cbind(rmse, mae, corr) ~ variant, folds, mean)

calibration <- do.call(rbind, lapply(variants, function(v) {
  x <- readv(v, "situation_v3_calibration_2025.csv")
  x$variant <- v
  x
}))

# ---- Same-game 30/30/40 fair-margin diagnostic ----
margin_rows <- lapply(variants, function(v) {
  x <- readv(v, "situation_v3_2025_team_games.csv")

  home <- x[x$sit_team == x$home_team,
            c("game_id","home_team","away_team","actual_points","fair_30_30_40")]
  away <- x[x$sit_team == x$away_team,
            c("game_id","actual_points","fair_30_30_40")]

  names(home)[4:5] <- c("home_actual","home_fair")
  names(away)[2:3] <- c("away_actual","away_fair")
  g <- merge(home, away, by="game_id")

  actual_margin <- g$home_actual - g$away_actual
  fair_margin   <- g$home_fair - g$away_fair
  err <- fair_margin - actual_margin

  data.frame(
    variant=v,
    games=nrow(g),
    margin_rmse=sqrt(mean(err^2)),
    margin_mae=mean(abs(err)),
    margin_corr=cor(fair_margin, actual_margin)
  )
})
margin_summary <- do.call(rbind, margin_rows)

# ---- Direct A/B comparison ----
cur_f <- fold_summary[fold_summary$variant=="current_60",]
pro_f <- fold_summary[fold_summary$variant=="proposed_100_70",]
cur_m <- margin_summary[margin_summary$variant=="current_60",]
pro_m <- margin_summary[margin_summary$variant=="proposed_100_70",]

ab <- data.frame(
  metric=c("CV situation RMSE","CV situation MAE","CV situation correlation",
           "30/30/40 margin RMSE","30/30/40 margin MAE","30/30/40 margin correlation"),
  current=c(cur_f$rmse,cur_f$mae,cur_f$corr,
            cur_m$margin_rmse,cur_m$margin_mae,cur_m$margin_corr),
  proposed=c(pro_f$rmse,pro_f$mae,pro_f$corr,
             pro_m$margin_rmse,pro_m$margin_mae,pro_m$margin_corr)
)
ab$change <- ab$proposed - ab$current
ab$better <- c(ab$change[1:2] < 0, ab$change[3] > 0,
               ab$change[4:5] < 0, ab$change[6] > 0)

write.csv(folds, "canonical_second_down_AB/fold_results.csv", row.names=FALSE)
write.csv(fold_summary, "canonical_second_down_AB/fold_summary.csv", row.names=FALSE)
write.csv(calibration, "canonical_second_down_AB/calibration_comparison.csv", row.names=FALSE)
write.csv(margin_summary, "canonical_second_down_AB/margin_summary.csv", row.names=FALSE)
write.csv(ab, "canonical_second_down_AB/AB_summary.csv", row.names=FALSE)

cat("\n============================================================\n")
cat("FINAL CANONICAL SECOND-DOWN TARGET A/B TEST\n")
cat("============================================================\n\n")
print(ab)
cat("\n'better = TRUE' means proposed wins that metric.\n")
cat("Negative RMSE/MAE change is good; positive correlation change is good.\n")
