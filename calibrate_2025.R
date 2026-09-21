options(stringsAsFactors = FALSE)

# ============================================================
# NFL NUMBERS — 2025 SITUATIONAL MODEL CALIBRATION
# PENALTY-AWARE VERSION
#
# Penalty rules:
#   football rush/pass yards                  = 100%
#   accepted live-possession penalty yards   = 50%
#   declined penalties                       = 0 additional effect
#   dead-ball/post-play/between-downs         = 0 penalty effect
#   kickoff/XP/non-offensive enforcement     = 0 offensive Situation effect
#   defensive penalties converting 3rd/4th   = preserve conversion benefit
#
# Existing rules retained:
#   any fumble = -1 event penalty
#   interception gain = 0, plus turnover penalty
#   kneel = 0
# ============================================================

SEASON <- 2025
PENALTY_YARD_WEIGHT <- 0.50

PBP_URL <- paste0(
  "https://github.com/nflverse/nflverse-data/releases/download/pbp/",
  "play_by_play_", SEASON, ".csv.gz"
)

dir.create("calibration_2025", showWarnings = FALSE)

dest <- "calibration_2025/play_by_play_2025.csv.gz"

if (!file.exists(dest)) {
  cat("Downloading 2025 nflverse play-by-play...\n")
  download.file(PBP_URL, dest, mode = "wb", quiet = FALSE)
}

cat("Reading play-by-play...\n")
pbp <- read.csv(gzfile(dest), stringsAsFactors = FALSE)
cat("Rows loaded:", nrow(pbp), "\n")

if ("season_type" %in% names(pbp))
  pbp <- pbp[pbp$season_type == "REG", ]

if ("week" %in% names(pbp))
  pbp <- pbp[pbp$week >= 1 & pbp$week <= 18, ]

cat("Regular-season rows:", nrow(pbp), "\n")

as_num <- function(x) suppressWarnings(as.numeric(x))

flag <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  !is.na(x) & as.character(x) %in% c("1", "TRUE", "true", "T")
}

txt_has <- function(x, pattern) {
  !is.na(x) & grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
}

play_value <- function(down, togo, gain,
                       turnover = FALSE,
                       kneel = FALSE,
                       force_conversion = FALSE) {

  if (isTRUE(kneel)) return(0)

  if (is.na(down) || is.na(togo) || is.na(gain) ||
      togo <= 0 || !(down %in% 1:4))
    return(NA_real_)

  target <- if (down == 1) {
    0.4 * togo
  } else if (down == 2) {
    0.6 * togo
  } else {
    togo
  }

  # A defensive accepted penalty can legally convert 3rd/4th down
  # even though the 50%-weighted effective yardage is < togo.
  gain_for_scoring <- gain

  if (isTRUE(force_conversion) &&
      down %in% c(3, 4) &&
      gain_for_scoring < togo) {
    gain_for_scoring <- togo
  }

  achievement <- gain_for_scoring / target

  if (down %in% c(3,4) &&
      gain_for_scoring < togo) {
    value <- max(-1.5, achievement - 1)
  } else if (achievement <= 1) {
    value <- max(-1.5, achievement)
  } else {
    value <- min(1.75, 1 + 0.35 * log(achievement))
  }

  if (isTRUE(turnover)) value <- value - 1

  value
}

required <- c(
  "game_id", "week", "posteam", "down", "ydstogo", "yards_gained"
)

missing_required <- setdiff(required, names(pbp))

if (length(missing_required)) {
  stop(
    "Missing required columns: ",
    paste(missing_required, collapse = ", ")
  )
}

q <- pbp

q$down_num <- as_num(q$down)
q$togo_num <- as_num(q$ydstogo)
q$football_gain <- as_num(q$yards_gained)

desc <- if ("desc" %in% names(q)) {
  as.character(q$desc)
} else {
  rep("", nrow(q))
}

# ------------------------------------------------------------
# PENALTY CLASSIFICATION
# ------------------------------------------------------------

penalty_flag <- rep(FALSE, nrow(q))
if ("penalty" %in% names(q))
  penalty_flag <- flag(q$penalty)

penalty_flag <- penalty_flag | txt_has(desc, "PENALTY")

penalty_yards <- rep(0, nrow(q))
if ("penalty_yards" %in% names(q)) {
  py <- as_num(q$penalty_yards)
  py[!is.finite(py)] <- 0
  penalty_yards <- abs(py)
}

penalty_team <- rep(NA_character_, nrow(q))
if ("penalty_team" %in% names(q))
  penalty_team <- as.character(q$penalty_team)

declined <- txt_has(
  desc,
  "declined|offsetting|offset penalties|penalties offset"
)

dead_ball <- txt_has(
  desc,
  paste0(
    "dead ball|between downs|after the play|after play|",
    "enforced on (the )?kickoff|ensuing kickoff|",
    "during the try|on the try|extra point|PAT"
  )
)

# nflverse yards_gained is football yardage; penalty_yards is separate.
# Therefore do NOT subtract penalty yards from yards_gained and add them
# back. Start with football yards and add only the weighted consequence.

live_accepted_penalty <-
  penalty_flag &
  !declined &
  !dead_ball &
  penalty_yards > 0

offensive_penalty <-
  live_accepted_penalty &
  !is.na(penalty_team) &
  penalty_team == q$posteam

defensive_penalty <-
  live_accepted_penalty &
  !is.na(penalty_team) &
  penalty_team != "" &
  penalty_team != q$posteam

# Fallback only for rows where nflverse identifies a live penalty but
# penalty_team is missing. We retain the football play but do not invent
# the direction of the penalty. These are written to the audit for review.
unknown_penalty_side <-
  live_accepted_penalty &
  !(offensive_penalty | defensive_penalty)

penalty_effect <- rep(0, nrow(q))
penalty_effect[offensive_penalty] <-
  -PENALTY_YARD_WEIGHT * penalty_yards[offensive_penalty]
penalty_effect[defensive_penalty] <-
   PENALTY_YARD_WEIGHT * penalty_yards[defensive_penalty]

q$effective_gain <- q$football_gain + penalty_effect

# ------------------------------------------------------------
# NO-PLAY HANDLING
# ------------------------------------------------------------

no_play <- rep(FALSE, nrow(q))
if ("no_play" %in% names(q))
  no_play <- flag(q$no_play)

no_play <- no_play | txt_has(desc, "No Play|NO PLAY")

# IMPORTANT:
# Do not throw away accepted live-possession penalty rows merely because
# nflverse labels the snap "no_play". Those penalty consequences are part
# of the Situation model. For a genuine no-play penalty there are no
# football yards, so score only 50% of the enforced penalty yardage.
accepted_penalty_no_play <-
  no_play &
  live_accepted_penalty &
  (offensive_penalty | defensive_penalty)

q$effective_gain[accepted_penalty_no_play] <-
  penalty_effect[accepted_penalty_no_play]

q$football_gain[accepted_penalty_no_play] <- 0

# Declined/dead-ball no-play rows contribute nothing and remain excluded.
exclude_no_play <- no_play & !accepted_penalty_no_play

# ------------------------------------------------------------
# SPECIAL TEAMS / KNEELS / TURNOVERS
# ------------------------------------------------------------

special <- rep(FALSE, nrow(q))
if ("special_teams_play" %in% names(q))
  special <- flag(q$special_teams_play)

kneel <- rep(FALSE, nrow(q))
if ("qb_kneel" %in% names(q))
  kneel <- flag(q$qb_kneel)
kneel <- kneel | txt_has(desc, "kneel|kneels")

interception <- rep(FALSE, nrow(q))
if ("interception" %in% names(q))
  interception <- flag(q$interception)
if ("interception_player_id" %in% names(q)) {
  interception <- interception |
    (!is.na(q$interception_player_id) &
       q$interception_player_id != "")
}

fumble <- rep(FALSE, nrow(q))
if ("fumble" %in% names(q))
  fumble <- flag(q$fumble)
if ("fumbled_1_player_id" %in% names(q)) {
  fumble <- fumble |
    (!is.na(q$fumbled_1_player_id) &
       q$fumbled_1_player_id != "")
}
fumble <- fumble | txt_has(desc, "FUMBLES|Fumble")

# Interception: offensive gain is zero. Retain any legitimate live
# defensive penalty consequence separately.
q$effective_gain[interception] <- penalty_effect[interception]
q$football_gain[interception] <- 0

turnover_event <- interception | fumble

# ------------------------------------------------------------
# PRESERVE PENALTY FIRST-DOWN CONVERSIONS
# ------------------------------------------------------------

first_down_penalty <- rep(FALSE, nrow(q))
if ("first_down_penalty" %in% names(q))
  first_down_penalty <- flag(q$first_down_penalty)

automatic_first_down <- txt_has(
  desc,
  "automatic first down"
)

force_conversion <-
  defensive_penalty &
  q$down_num %in% c(3, 4) &
  (first_down_penalty | automatic_first_down)

# A defensive penalty can also convert by distance without being described
# as "automatic". If the full enforced penalty alone reaches the line to
# gain, preserve the conversion even though we value those penalty yards
# at only 50%.
force_conversion <-
  force_conversion |
  (
    defensive_penalty &
    q$down_num %in% c(3, 4) &
    penalty_yards >= q$togo_num
  )

# ------------------------------------------------------------
# KEEP OFFENSIVE SITUATION EVENTS
# ------------------------------------------------------------

keep <- (
  !exclude_no_play &
  !special &
  !is.na(q$posteam) &
  q$posteam != "" &
  !is.na(q$down_num) &
  q$down_num %in% 1:4 &
  !is.na(q$togo_num) &
  q$togo_num > 0 &
  !is.na(q$effective_gain)
)

q <- q[keep, ]
kneel <- kneel[keep]
turnover_event <- turnover_event[keep]
interception <- interception[keep]
fumble <- fumble[keep]
penalty_flag <- penalty_flag[keep]
penalty_yards <- penalty_yards[keep]
penalty_team <- penalty_team[keep]
declined <- declined[keep]
dead_ball <- dead_ball[keep]
live_accepted_penalty <- live_accepted_penalty[keep]
offensive_penalty <- offensive_penalty[keep]
defensive_penalty <- defensive_penalty[keep]
unknown_penalty_side <- unknown_penalty_side[keep]
penalty_effect <- penalty_effect[keep]
force_conversion <- force_conversion[keep]
accepted_penalty_no_play <- accepted_penalty_no_play[keep]

cat("Scored offensive plays:", nrow(q), "\n")
cat("Accepted live penalty plays:",
    sum(live_accepted_penalty), "\n")
cat("Accepted penalty no-plays retained:",
    sum(accepted_penalty_no_play), "\n")
cat("Penalty conversion overrides:",
    sum(force_conversion), "\n")
cat("Live penalties with unknown side:",
    sum(unknown_penalty_side), "\n")

# ------------------------------------------------------------
# SCORE
# ------------------------------------------------------------

q$play_value <- mapply(
  play_value,
  q$down_num,
  q$togo_num,
  q$effective_gain,
  turnover_event,
  kneel,
  force_conversion
)

q$turnover_event <- turnover_event
q$interception_event <- interception
q$fumble_event <- fumble
q$penalty_event <- penalty_flag
q$penalty_yards_model <- penalty_yards
q$penalty_team_model <- penalty_team
q$penalty_declined <- declined
q$penalty_dead_ball <- dead_ball
q$penalty_live_accepted <- live_accepted_penalty
q$penalty_offense <- offensive_penalty
q$penalty_defense <- defensive_penalty
q$penalty_side_unknown <- unknown_penalty_side
q$penalty_effective_yards <- penalty_effect
q$penalty_conversion_override <- force_conversion
q$penalty_no_play_retained <- accepted_penalty_no_play

write.csv(
  q,
  "calibration_2025/2025_scored_plays_penalty_aware.csv",
  row.names = FALSE
)

# Separate penalty audit: this is useful for checking the classification.
write.csv(
  q[q$penalty_event, ],
  "calibration_2025/2025_penalty_audit.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# TEAM-GAME RAW TOTALS
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
  raw, plays,
  by = c("game_id", "week", "posteam"),
  all = TRUE
)

team_game$raw_per_play <-
  team_game$raw_situation / team_game$scored_plays

# ------------------------------------------------------------
# ACTUAL POINTS
# ------------------------------------------------------------

if (!all(c(
  "home_team", "away_team",
  "total_home_score", "total_away_score"
) %in% names(pbp))) {
  stop("Could not identify final score columns.")
}

games <- unique(
  pbp[, c(
    "game_id", "week", "home_team", "away_team",
    "total_home_score", "total_away_score"
  )]
)

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

  actual_rows[[length(actual_rows) + 1]] <- data.frame(
    game_id = g$game_id,
    week = g$week,
    posteam = g$home_team,
    actual_points = as.numeric(g$total_home_score)
  )

  actual_rows[[length(actual_rows) + 1]] <- data.frame(
    game_id = g$game_id,
    week = g$week,
    posteam = g$away_team,
    actual_points = as.numeric(g$total_away_score)
  )
}

actual <- do.call(rbind, actual_rows)

team_game <- merge(
  team_game, actual,
  by = c("game_id", "week", "posteam"),
  all.x = TRUE
)

# ------------------------------------------------------------
# QUALITY CHECKS
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
# RECALIBRATION
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

# Candidate A: preserve the shape of the Situation model and rescale it.
multiplier_mean <- points_mean / raw_mean

fit_zero <- lm(
  actual_points ~ 0 + raw_situation,
  data = team_game
)
multiplier_ls <- unname(coef(fit_zero)[1])

# For comparison only. We are not automatically changing the model to an
# intercept regression.
fit_intercept <- lm(
  actual_points ~ raw_situation,
  data = team_game
)

intercept_a <- unname(coef(fit_intercept)[1])
slope_b <- unname(coef(fit_intercept)[2])

fit_volume <- lm(
  actual_points ~ raw_situation + scored_plays,
  data = team_game
)

# OOS comparison: fit Weeks 1-12, test Weeks 13-18.
train <- team_game[team_game$week <= 12, ]
test  <- team_game[team_game$week >= 13, ]

fit_oos_mean_zero <- lm(
  actual_points ~ 0 + raw_situation,
  data = train
)

fit_oos_intercept <- lm(
  actual_points ~ raw_situation,
  data = train
)

fit_oos_volume <- lm(
  actual_points ~ raw_situation + scored_plays,
  data = train
)

test$pred_zero <- predict(fit_oos_mean_zero, newdata = test)
test$pred_intercept <- predict(fit_oos_intercept, newdata = test)
test$pred_volume <- predict(fit_oos_volume, newdata = test)

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

summary_lines <- c(
  "NFL NUMBERS — 2025 PENALTY-AWARE SITUATIONAL CALIBRATION",
  "========================================================",
  "",
  paste("Regular-season team-games:", nrow(team_game)),
  paste("Penalty-yard weight:", PENALTY_YARD_WEIGHT),
  "",
  "PENALTY AUDIT",
  paste("Accepted live penalty plays:", sum(q$penalty_live_accepted)),
  paste("Accepted penalty no-plays retained:", sum(q$penalty_no_play_retained)),
  paste("Penalty conversion overrides:", sum(q$penalty_conversion_override)),
  paste("Live penalties with unknown side:", sum(q$penalty_side_unknown)),
  "",
  "RAW SITUATIONAL DISTRIBUTION",
  paste("Mean:", round(raw_mean, 6)),
  paste("Median:", round(raw_median, 6)),
  paste("SD:", round(raw_sd, 6)),
  paste(
    "Quantiles:",
    paste(
      names(raw_quantiles),
      round(raw_quantiles, 4),
      collapse = " | "
    )
  ),
  paste("Mean actual points:", round(points_mean, 6)),
  paste(
    "Correlation raw situation vs actual points:",
    round(cor_raw_points, 6)
  ),
  "",
  "SCALE-ONLY CANDIDATES",
  paste(
    "Mean-matching multiplier:",
    sprintf("%.8f", multiplier_mean)
  ),
  paste(
    "Least-squares zero-intercept multiplier:",
    sprintf("%.8f", multiplier_ls)
  ),
  "",
  "INTERCEPT REGRESSION — COMPARISON ONLY",
  paste(
    "Situational points =",
    round(intercept_a, 6),
    "+",
    round(slope_b, 6),
    "x raw situation"
  ),
  "",
  "OUT-OF-SAMPLE WEEKS 13-18",
  paste(
    "Zero-intercept RMSE:",
    round(rmse(test$actual_points, test$pred_zero), 6)
  ),
  paste(
    "Zero-intercept MAE:",
    round(mae(test$actual_points, test$pred_zero), 6)
  ),
  paste(
    "Intercept regression RMSE:",
    round(rmse(test$actual_points, test$pred_intercept), 6)
  ),
  paste(
    "Intercept regression MAE:",
    round(mae(test$actual_points, test$pred_intercept), 6)
  ),
  paste(
    "Raw + play-volume RMSE:",
    round(rmse(test$actual_points, test$pred_volume), 6)
  ),
  paste(
    "Raw + play-volume MAE:",
    round(mae(test$actual_points, test$pred_volume), 6)
  ),
  "",
  "IMPORTANT",
  "Do not automatically adopt the intercept/volume model.",
  "The scale-only candidates preserve the established Situation metric shape.",
  "Review the penalty audit and calibration results before selecting the new multiplier."
)

write.csv(
  team_game,
  "calibration_2025/team_game_calibration_penalty_aware.csv",
  row.names = FALSE
)

write.csv(
  test,
  "calibration_2025/out_of_sample_weeks13_18_penalty_aware.csv",
  row.names = FALSE
)

writeLines(
  summary_lines,
  "calibration_2025/calibration_summary_penalty_aware.txt"
)

cat("\n")
cat(paste(summary_lines, collapse = "\n"))
cat("\n\nSUCCESS — penalty-aware 2025 calibration complete.\n")
