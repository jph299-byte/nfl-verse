options(stringsAsFactors = FALSE)

# NFL Numbers — Situation v3 rebuild / calibration
# Locked rules 2026-09-22.
#
# This script is intentionally a scoring layer, not an app updater.
# It reads the exact-play CSVs produced by convert_week.R and rebuilds
# Situation under the locked rules. It can therefore be audited before
# the production converter is replaced.
#
# Required input:
#   weeks1-2-locked-situation.zip extracted in the working directory,
#   OR pass its extracted directory as arg 1.
#
# Outputs:
#   situation_v3/week1_v3_plays.csv
#   situation_v3/week2_v3_plays.csv
#   situation_v3/week1_v3_team_raw.csv
#   situation_v3/week2_v3_team_raw.csv
#   situation_v3/CAR_ATL_v3_play_by_play.csv
#   situation_v3/audit_edge_cases.csv
#
# IMPORTANT:
# 2025 recalibration must be run from raw 2025 play data after the same
# classifier is wired into convert_week.R. This script deliberately does
# not pretend old exact-play CSVs contain punts/kickoffs that were filtered
# out upstream.

args <- commandArgs(trailingOnly=TRUE)
ROOT <- if (length(args) >= 1) args[1] else "."
OUT <- file.path(ROOT, "situation_v3")
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)

num <- function(x) suppressWarnings(as.numeric(x))
txt <- function(x) { x[is.na(x)] <- ""; x }

# Final ruling: if a replay reversal exists, classify the final ruling only.
final_ruling <- function(x) {
  x <- txt(as.character(x))
  vapply(x, function(s) {
    m <- gregexpr("(?i)REVERSED\\.", s, perl=TRUE)[[1]]
    if (m[1] == -1) return(s)
    ml <- attr(m, "match.length")
    k <- length(m)
    substr(s, m[k] + ml[k], nchar(s))
  }, character(1))
}

# Negative yardage: diminishing gradient /4.
negative_value <- function(gain) {
  if (!is.finite(gain) || gain >= 0) return(0)
  -0.35 * log(1 + abs(gain)/4)
}

# Positive play value.
# 1st target = 40% to-go; 2nd = 60%; 3rd = to-go.
# 3rd down is NOT possession-ending.
# Short-yardage successful conversion/excess uses /8.
positive_scrimmage_value <- function(down, togo, gain) {
  if (!is.finite(down) || !is.finite(togo) || !is.finite(gain) ||
      togo <= 0 || !(down %in% 1:4)) return(NA_real_)
  if (gain < 0) return(negative_value(gain))
  target <- if (down == 1) .4*togo else if (down == 2) .6*togo else togo
  if (gain == 0) return(0)

  # 4th down is the possession decision.
  if (down == 4) {
    if (gain < togo) return(-1)
    excess <- max(0, gain - togo)
    return(min(1.75, 1 + .35*log(1 + excess/8)))
  }

  # Locked short-yardage rule: successful 2nd & 1-4 and successful 3rd down.
  if ((down == 2 && togo <= 4 && gain >= togo) ||
      (down == 3 && gain >= togo)) {
    excess <- max(0, gain - togo)
    return(min(1.75, 1 + .35*log(1 + excess/8)))
  }

  # Ordinary positive achievement. Falling short on 3rd does NOT incur
  # a turnover-style -1; it is simply the football gain achieved.
  r <- gain/target
  if (r <= 1) return(r)
  min(1.75, 1 + .35*log(r))
}

classify <- function(desc) {
  d <- final_ruling(desc)
  dl <- tolower(d)

  penalty <- grepl("penalty", dl)
  declined <- grepl("declined", dl)
  offset <- grepl("offsetting|offset", dl)
  dpi <- penalty && grepl("pass interference", dl) &&
    !grepl("offensive pass interference", dl)

  kickoff <- grepl("kicks? off|kickoff", dl)
  punt <- grepl("\\bpunt(s|ed|ing)?\\b", dl)
  fg <- grepl("field goal|field-goal", dl)
  fg_good <- fg && grepl("\\bgood\\b", dl) && !grepl("no good|blocked", dl)
  fg_bad <- fg && grepl("no good|missed|blocked|aborted", dl)

  kneel <- grepl("kneel", dl)
  spike <- grepl("spike", dl)

  interception <- grepl("intercepted|interception", dl)
  fumble <- grepl("fumble", dl)

  # TD must be a final-ruling score, not text from a reversed ruling.
  td <- grepl("touchdown", dl)
  safety_score <- grepl("\\bsafety\\b", dl) &&
    !grepl("safety [A-Z][a-z]|free safety|strong safety", d, perl=TRUE)

  list(ruling=d, penalty=penalty, declined=declined, offset=offset, dpi=dpi,
       kickoff=kickoff, punt=punt, fg=fg, fg_good=fg_good, fg_bad=fg_bad,
       kneel=kneel, spike=spike, interception=interception, fumble=fumble,
       td=td, safety=safety_score)
}

extract_penalty_yards <- function(s) {
  s <- final_ruling(s)
  # Prefer explicit penalty enforcement yards.
  p <- regmatches(s, regexpr("(?i)PENALTY.*?([0-9]+) yards?", s, perl=TRUE))
  if (!length(p) || is.na(p) || p=="") return(NA_real_)
  m <- regmatches(p, gregexpr("[0-9]+", p))[[1]]
  if (!length(m)) return(NA_real_)
  as.numeric(tail(m,1))
}

# Heuristic: accepted ordinary penalty/no-play = 0.  Declined penalty means
# score the football play normally.  DPI is the sole valued penalty and uses
# half the enforced yardage as effective gain.
score_row <- function(row) {
  d <- as.character(row[["description"]])
  c <- classify(d)
  down <- num(row[["down"]]); togo <- num(row[["yards_to_go"]])
  football_gain <- if ("football_gain" %in% names(row)) num(row[["football_gain"]]) else num(row[["raw_gain"]])
  if (!is.finite(football_gain)) football_gain <- num(row[["raw_gain"]])
  if (!is.finite(football_gain)) football_gain <- 0

  # Kneels/spikes.
  if (c$kneel || c$spike) base <- 0
  else if (c$kickoff) base <- 0
  else if (c$punt) base <- -1
  else if (c$fg_good) base <- 1
  else if (c$fg_bad || c$fg) base <- -1
  else if (c$penalty && !c$declined && !c$offset && !c$dpi) base <- 0
  else if (c$offset) base <- 0
  else if (c$dpi && !c$declined) {
    py <- extract_penalty_yards(d)
    eff <- if (is.finite(py)) .5*py else 0
    base <- positive_scrimmage_value(down, togo, eff)
  } else {
    base <- positive_scrimmage_value(down, togo, football_gain)
  }

  if (!is.finite(base)) base <- 0

  # Turnover event applies only if it survives in the final ruling.
  # A fumble is -1 regardless of recovery, per locked model.
  if (!c$penalty || c$declined) {
    if (c$interception) base <- base - 1
    if (c$fumble) base <- base - 1
  }

  # Offensive TD bonus. For existing exact-play ledgers, possession team is
  # offence; defensive-return TD text usually follows an interception/fumble.
  offensive_td <- c$td && !c$interception && !grepl("returned.*touchdown|return.*touchdown",
                                                     tolower(c$ruling))
  if (offensive_td) base <- base + 1

  # Return TD against kicking team.
  if (c$punt && c$td) base <- -2
  if (c$kickoff && c$td) base <- -1

  # Genuine scoring safety: -1 on top of underlying offensive play.
  if (c$safety) base <- base - 1

  list(value=base, ruling=c$ruling,
       play_type=if(c$kickoff)"KICKOFF" else if(c$punt)"PUNT" else
                 if(c$fg)"FIELD_GOAL" else if(c$dpi)"DPI" else
                 if(c$penalty)"PENALTY" else "SCRIMMAGE",
       td_bonus=as.integer(offensive_td),
       safety_penalty=as.integer(c$safety),
       dpi=as.integer(c$dpi))
}

find_play_files <- function(root, week) {
  p <- file.path(root, paste0("week",week), "audit", "plays")
  list.files(p, pattern="_exact_plays\\.csv$", full.names=TRUE)
}

process_week <- function(week) {
  fs <- find_play_files(ROOT, week)
  if (!length(fs)) stop("No exact-play CSVs found for week ",week," under ",ROOT)
  all <- list()
  for (f in fs) {
    z <- read.csv(f, check.names=FALSE)
    game <- sub("_exact_plays\\.csv$","",basename(f))
    if (!"description" %in% names(z)) stop("description missing: ",f)
    sc <- lapply(seq_len(nrow(z)), function(i) score_row(as.list(z[i,,drop=FALSE])))
    z$game <- game
    z$v3_final_ruling <- vapply(sc, `[[`, character(1), "ruling")
    z$v3_play_type <- vapply(sc, `[[`, character(1), "play_type")
    z$v3_td_bonus <- vapply(sc, `[[`, integer(1), "td_bonus")
    z$v3_safety_penalty <- vapply(sc, `[[`, integer(1), "safety_penalty")
    z$v3_dpi <- vapply(sc, `[[`, integer(1), "dpi")
    z$v3_play_value <- vapply(sc, `[[`, numeric(1), "value")
    all[[length(all)+1]] <- z
  }
  out <- do.call(rbind, all)
  write.csv(out, file.path(OUT,paste0("week",week,"_v3_plays.csv")),row.names=FALSE)

  teams <- aggregate(v3_play_value ~ game + team, out, sum)
  names(teams)[3] <- "raw_situation_v3"
  write.csv(teams,file.path(OUT,paste0("week",week,"_v3_team_raw.csv")),row.names=FALSE)
  out
}

w1 <- process_week(1)
w2 <- process_week(2)

ca <- w2[w2$game=="CAR_ATL",,drop=FALSE]
if (nrow(ca)) {
  ca$ATL_cumulative_v3 <- cumsum(ifelse(ca$team=="ATL",ca$v3_play_value,0))
  ca$CAR_cumulative_v3 <- cumsum(ifelse(ca$team=="CAR",ca$v3_play_value,0))
  write.csv(ca,file.path(OUT,"CAR_ATL_v3_play_by_play.csv"),row.names=FALSE)
}

# Edge-case audit for manual inspection.
both <- rbind(w1,w2)
edge <- grepl("penalty|intercept|fumble|touchdown|safety|field goal|punt|kickoff|kicks off|aborted|reversed",
              both$description, ignore.case=TRUE)
write.csv(both[edge,,drop=FALSE],file.path(OUT,"audit_edge_cases.csv"),row.names=FALSE)

cat("\nSituation v3 audit rebuild complete.\n")
cat("NOTE: old exact-play CSVs filtered normal punts/kickoffs upstream.\n")
cat("Therefore these outputs are for rule validation only, NOT final calibration.\n")
cat("Next production step: wire the same classifier before special-teams filtering,\n")
cat("then rebuild raw 2025 + 2026 data and recalibrate.\n")
