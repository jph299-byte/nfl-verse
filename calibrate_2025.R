options(stringsAsFactors = FALSE)

# ============================================================
# NFL NUMBERS — 2025 SITUATIONAL MODEL CALIBRATION
#
# Purpose:
#   Apply the established NFL Numbers situational play formula
#   to the complete 2025 REGULAR SEASON and determine how raw
#   situational units should convert to NFL points.
#
# IMPORTANT:
#   This does NOT alter the play-scoring model.
# ============================================================

SEASON <- 2025

PBP_URL <- paste0(
  "https://github.com/nflverse/nflverse-data/releases/download/pbp/",
  "play_by_play_", SEASON, ".csv.gz"
)

dir.create("calibration_2025", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Download 2025 nflverse PBP
# ------------------------------------------------------------

dest <- "calibration_2025/play_by_play_2025.csv.gz"

if (!file.exists(dest)) {
  cat("Downloading 2025 nflverse play-by-play...\n")
  download.file(PBP_URL, dest, mode = "wb", quiet = FALSE)
}

cat("Reading play-by-play...\n")
pbp <- read.csv(gzfile(dest), stringsAsFactors = FALSE)

cat("Rows loaded:", nrow(pbp), "\n")

# ------------------------------------------------------------
# 2. Keep REGULAR SEASON only
# ------------------------------------------------------------

if ("season_type" %in% names(pbp)) {
  pbp <- pbp[pbp$season_type == "REG", ]
}

if ("week" %in% names(pbp)) {
  pbp <- pbp[pbp$week >= 1 & pbp$week <= 18, ]
}

cat("Regular-season rows:", nrow(pbp), "\n")

# ------------------------------------------------------------
# 3. Helpers
# ------------------------------------------------------------

as_num <- function(x) suppressWarnings(as.numeric(x))

flag <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  !is.na(x) & as.character(x) %in% c("1", "TRUE", "true", "T")
}

# EXACT CURRENT NFL NUMBERS PLAY FORMULA

play_value <- function(down, togo, gain,
                       turnover = FALSE,
                       kneel = FALSE) {

  if (isTRUE(kneel)) return(0)

  if (
    is.na(down) ||
    is.na(togo) ||
    is.na(gain) ||
    togo <= 0 ||
    !(down %in% 1:4)
  ) return(NA_real_)

  target <- if (down == 1) {
    0.4 * togo
  } else if (down == 2) {
    0.6 * togo
  } else {
    togo
  }

  achievement <- gain / target

  if (down %in% c(3,4) && gain < togo) {

    value <- max(-1.5, achievement - 1)

  } else if (achievement <= 1) {

    value <- max(-1.5, achievement)

  } else {

    value <- min(
      1.75,
      1 + 0.35 * log(achievement)
    )
  }

  if (isTRUE(turnover)) value <- value - 1

  value
}

# ------------------------------------------------------------
# 4. Identify required nflverse columns
# ------------------------------------------------------------

required <- c(
  "game_id",
  "week",
  "posteam",
  "down",
  "ydstogo",
  "yards_gained"
)

missing_required <- setdiff(required, names(pbp))

if (length(missing_required)) {
  stop(
    "Missing required columns: ",
    paste(missing_required, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 5. Build scoring-play dataset
# ------------------------------------------------------------

q <- pbp

q$down_num <- as_num(q$down)
q$togo_num <- as_num(q$ydstogo)
q$gain_num <- as_num(q$yards_gained)

# Exclude no-plays where available
no_play <- rep(FALSE, nrow(q))

if ("no_play" %in% names(q)) {
  no_play <- flag(q$no_play)
}

if ("desc" %in% names(q)) {
  no_play <- no_play |
    grepl("No Play|NO PLAY", q$desc)
}

# Exclude special-teams plays
special <- rep(FALSE, nrow(q))

if ("special_teams_play" %in% names(q)) {
  special <- flag(q$special_teams_play)
}

# Kneels score zero
kneel <- rep(FALSE, nrow(q))

if ("qb_kneel" %in% names(q)) {
  kneel <- flag(q$qb_kneel)
}

if ("desc" %in% names(q)) {
  kneel <- kneel |
    grepl("kneel|kneels", q$desc, ignore.case = TRUE)
}

# Interceptions
interception <- rep(FALSE, nrow(q))

if ("interception" %in% names(q)) {
  interception <- flag(q$interception)
}

if ("interception_player_id" %in% names(q)) {
  interception <- interception |
    (!is.na(q$interception_player_id) &
       q$interception_player_id != "")
}

# Fumbles
#
# IMPORTANT:
# Preserve the existing 2026 model:
# ANY fumble gets the -1 event penalty,
# whether recovered by offence or defence.

fumble <- rep(FALSE, nrow(q))

if ("fumble" %in% names(q)) {
  fumble <- flag(q$fumble)
}

if ("fumbled_1_player_id" %in% names(q)) {
  fumble <- fumble |
    (!is.na(q$fumbled_1_player_id) &
       q$fumbled_1_player_id != "")
}

if ("desc" %in% names(q)) {
  fumble <- fumble |
    grepl("FUMBLES|Fumble", q$desc, ignore.case = TRUE)
}

# Same interception treatment as live converter:
# offensive gain reset to zero.

q$gain_model <- q$gain_num
q$gain_model[interception] <- 0

turnover_event <- interception | fumble

keep <- (
  !no_play &
  !special &
  !is.na(q$posteam) &
  q$posteam != "" &
  !is.na(q$down_num) &
  q$down_num %in% 1:4 &
  !is.na(q$togo_num) &
  q$togo_num > 0 &
  !is.na(q$gain_model)
)

q <- q[keep, ]

kneel <- kneel[keep]
turnover_event <- turnover_event[keep]
interception <- interception[keep]
fumble <- fumble[keep]

cat("Scored offensive plays:", nrow(q), "\n")

# ------------------------------------------------------------
# 6. Score every play
# ------------------------------------------------------------

q$play_value <- mapply(
  play_value,
  q$down_num,
  q$togo_num,
  q$gain_model,
  turnover_event,
  kneel
)

q$turnover_event <- turnover_event
q$interception_event <- interception
q$fumble_event <- fumble

write.csv(
  q,
  "calibration_2025/2025_scored_plays.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. Team-game raw situational totals
# ------------------------------------------------------------

raw <- aggregate(
  play_value ~ game_id + week + posteam,
  data = q,
  FUN = sum,
  na.rm = TRUE
)

names(raw)[names(raw) == "play_value"] <- "raw_situation"

plays <- aggregate(
  play_value ~ game_id + week + posteam,
  data = q,
  FUN = length
)

names(plays)[names(plays) == "play_value"] <- "scored_plays"

team_game <- merge(
  raw,
  plays,
  by = c("game_id", "week", "posteam"),
  all = TRUE
)

team_game$raw_per_play <-
  team_game$raw_situation / team_game$scored_plays

# ------------------------------------------------------------
# 8. Actual offensive points
# ------------------------------------------------------------

# nflverse total_home_score / total_away_score are final
# scoreboard totals repeated on the game rows.

if (!all(c(
  "home_team",
  "away_team",
  "total_home_score",
  "total_away_score"
) %in% names(pbp))) {

  stop("Could not identify final score columns.")
}

games <- unique(
  pbp[, c(
    "game_id",
    "week",
    "home_team",
    "away_team",
    "total_home_score",
    "total_away_score"
  )]
)

# Some PBP versions can contain repeated score states.
# Take the maximum/final score for each game.

game_scores <- aggregate(
  cbind(total_home_score, total_away_score) ~
    game_id + week + home_team + away_team,
  data = games,
  FUN = max,
  na.rm = TRUE
)

actual_rows <- list()

for (i in seq_len(nrow(game_scores))) {

  g <- game_scores[i, ]

  actual_rows[[length(actual_rows) + 1]] <-
    data.frame(
      game_id = g$game_id,
      week = g$week,
      posteam = g$home_team,
      actual_points = as.numeric(g$total_home_score)
    )

  actual_rows[[length(actual_rows) + 1]] <-
    data.frame(
      game_id = g$game_id,
      week = g$week,
      posteam = g$away_team,
      actual_points = as.numeric(g$total_away_score)
    )
}

actual <- do.call(rbind, actual_rows)

team_game <- merge(
  team_game,
  actual,
  by = c("game_id", "week", "posteam"),
  all.x = TRUE
)

# ------------------------------------------------------------
# 9. Net offensive yards
# ------------------------------------------------------------

# nflverse has drive/play yardage, but for this calibration
# we do NOT want to quietly pretend summed play gains are
# official net offensive yards.
#
# Use team game statistics if present in the PBP schema.
# Otherwise the calibration can still run; yardage comparison
# will be marked unavailable rather than fabricated.

yard_candidates <- c(
  "total_yards",
  "net_yards",
  "offense_yards",
  "team_yards"
)

yard_col <- yard_candidates[
  yard_candidates %in% names(pbp)
]

if (length(yard_col)) {

  yc <- yard_col[1]

  yard_data <- pbp[
    !is.na(pbp$posteam) & pbp$posteam != "",
    c("game_id", "week", "posteam", yc)
  ]

  names(yard_data)[4] <- "net_offensive_yards"

  yard_data$net_offensive_yards <-
    as_num(yard_data$net_offensive_yards)

  yard_data <- aggregate(
    net_offensive_yards ~ game_id + week + posteam,
    data = yard_data,
    FUN = max,
    na.rm = TRUE
  )

  team_game <- merge(
    team_game,
    yard_data,
    by = c("game_id", "week", "posteam"),
    all.x = TRUE
  )

} else {

  team_game$net_offensive_yards <- NA_real_
}

team_game$yardage_fair_points <-
  team_game$net_offensive_yards / 14.5

# ------------------------------------------------------------
# 10. Basic quality checks
# ------------------------------------------------------------

team_game <- team_game[
  is.finite(team_game$raw_situation) &
  is.finite(team_game$actual_points),
]

cat("\nTeam-games available:", nrow(team_game), "\n")

if (nrow(team_game) != 544) {
  warning(
    "Expected 544 regular-season team-games; found ",
    nrow(team_game)
  )
}

# ------------------------------------------------------------
# 11. Descriptive statistics
# ------------------------------------------------------------

raw_mean <- mean(team_game$raw_situation)
raw_median <- median(team_game$raw_situation)
raw_sd <- sd(team_game$raw_situation)

raw_quantiles <- quantile(
  team_game$raw_situation,
  probs = c(.05, .10, .25, .50, .75, .90, .95)
)

points_mean <- mean(team_game$actual_points)

cor_raw_points <- cor(
  team_game$raw_situation,
  team_game$actual_points,
  use = "complete.obs"
)

cor_raw_yards <- if (
  any(is.finite(team_game$net_offensive_yards))
) {
  cor(
    team_game$raw_situation,
    team_game$net_offensive_yards,
    use = "complete.obs"
  )
} else {
  NA_real_
}

# ------------------------------------------------------------
# 12. Candidate A — simple multiplier
# ------------------------------------------------------------

multiplier_mean <- points_mean / raw_mean

team_game$situ_points_multiplier <-
  team_game$raw_situation * multiplier_mean

# Least-squares multiplier constrained through zero.
fit_zero <- lm(
  actual_points ~ 0 + raw_situation,
  data = team_game
)

multiplier_ls <- unname(coef(fit_zero)[1])

team_game$situ_points_zero_reg <-
  team_game$raw_situation * multiplier_ls

# ------------------------------------------------------------
# 13. Candidate B — regression with intercept
# ------------------------------------------------------------

fit_intercept <- lm(
  actual_points ~ raw_situation,
  data = team_game
)

intercept_a <- unname(coef(fit_intercept)[1])
slope_b <- unname(coef(fit_intercept)[2])

team_game$situ_points_regression <-
  predict(fit_intercept, newdata = team_game)

# ------------------------------------------------------------
# 14. Candidate C — account for play volume
# ------------------------------------------------------------

fit_volume <- lm(
  actual_points ~ raw_situation + scored_plays,
  data = team_game
)

team_game$situ_points_volume <-
  predict(fit_volume, newdata = team_game)

# ------------------------------------------------------------
# 15. Out-of-sample test
#
# Fit Weeks 1-12
# Test Weeks 13-18
# ------------------------------------------------------------

train <- team_game[team_game$week <= 12, ]
test  <- team_game[team_game$week >= 13, ]

fit_oos_raw <- lm(
  actual_points ~ raw_situation,
  data = train
)

fit_oos_volume <- lm(
  actual_points ~ raw_situation + scored_plays,
  data = train
)

test$pred_raw <-
  predict(fit_oos_raw, newdata = test)

test$pred_volume <-
  predict(fit_oos_volume, newdata = test)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

oos_raw_rmse <- rmse(
  test$actual_points,
  test$pred_raw
)

oos_raw_mae <- mae(
  test$actual_points,
  test$pred_raw
)

oos_volume_rmse <- rmse(
  test$actual_points,
  test$pred_volume
)

oos_volume_mae <- mae(
  test$actual_points,
  test$pred_volume
)

# ------------------------------------------------------------
# 16. Detroit-Buffalo 2026 illustration
# ------------------------------------------------------------

DET_RAW <- 30.3
BUF_RAW <- 53.6

det_mean_mult <- DET_RAW * multiplier_mean
buf_mean_mult <- BUF_RAW * multiplier_mean

det_zero_reg <- DET_RAW * multiplier_ls
buf_zero_reg <- BUF_RAW * multiplier_ls

det_reg <- intercept_a + slope_b * DET_RAW
buf_reg <- intercept_a + slope_b * BUF_RAW

# Current known game components
DET_ACTUAL <- 31
BUF_ACTUAL <- 41

DET_YARD <- 355 / 14.5
BUF_YARD <- 446 / 14.5

det_fair_reg <-
  .30 * DET_ACTUAL +
  .30 * DET_YARD +
  .40 * det_reg

buf_fair_reg <-
  .30 * BUF_ACTUAL +
  .30 * BUF_YARD +
  .40 * buf_reg

# ------------------------------------------------------------
# 17. Output files
# ------------------------------------------------------------

write.csv(
  team_game,
  "calibration_2025/team_game_calibration.csv",
  row.names = FALSE
)

write.csv(
  test,
  "calibration_2025/out_of_sample_weeks13_18.csv",
  row.names = FALSE
)

summary_lines <- c(

  "NFL NUMBERS — 2025 SITUATIONAL CALIBRATION",
  "==========================================",
  "",

  paste("Regular-season team-games:", nrow(team_game)),
  "",

  "RAW SITUATIONAL DISTRIBUTION",
  paste("Mean:", round(raw_mean, 4)),
  paste("Median:", round(raw_median, 4)),
  paste("SD:", round(raw_sd, 4)),
  paste(
    "Quantiles:",
    paste(
      names(raw_quantiles),
      round(raw_quantiles, 4),
      collapse = " | "
    )
  ),
  "",

  "RELATIONSHIPS",
  paste(
    "Correlation raw situation vs actual points:",
    round(cor_raw_points, 4)
  ),
  paste(
    "Correlation raw situation vs net offensive yards:",
    round(cor_raw_yards, 4)
  ),
  "",

  "SCALE-ONLY CALIBRATION",
  paste(
    "Mean-matching multiplier:",
    round(multiplier_mean, 6)
  ),
  paste(
    "Least-squares zero-intercept multiplier:",
    round(multiplier_ls, 6)
  ),
  "",

  "REGRESSION WITH INTERCEPT",
  paste(
    "Situational points =",
    round(intercept_a, 6),
    "+",
    round(slope_b, 6),
    "x raw situation"
  ),
  "",

  "VOLUME MODEL",
  paste(
    capture.output(coef(fit_volume)),
    collapse = " "
  ),
  "",

  "OUT-OF-SAMPLE — WEEKS 13-18",
  paste(
    "Raw regression RMSE:",
    round(oos_raw_rmse, 4)
  ),
  paste(
    "Raw regression MAE:",
    round(oos_raw_mae, 4)
  ),
  paste(
    "Raw + play volume RMSE:",
    round(oos_volume_rmse, 4)
  ),
  paste(
    "Raw + play volume MAE:",
    round(oos_volume_mae, 4)
  ),
  "",

  "DETROIT-BUFFALO ILLUSTRATION",
  paste(
    "Mean multiplier DET / BUF:",
    round(det_mean_mult, 2),
    "/",
    round(buf_mean_mult, 2)
  ),
  paste(
    "Zero-regression DET / BUF:",
    round(det_zero_reg, 2),
    "/",
    round(buf_zero_reg, 2)
  ),
  paste(
    "Intercept regression DET / BUF:",
    round(det_reg, 2),
    "/",
    round(buf_reg, 2)
  ),
  paste(
    "30/30/40 fair score using intercept regression:",
    "DET",
    round(det_fair_reg, 2),
    "BUF",
    round(buf_fair_reg, 2)
  )
)

writeLines(
  summary_lines,
  "calibration_2025/calibration_summary.txt"
)

cat("\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\nSUCCESS — 2025 calibration complete.\n")
