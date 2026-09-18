options(stringsAsFactors = FALSE)

# ============================================================
# NFL NUMBERS — 2025 MARKET-BASED WEIGHT CALIBRATION
#
# Purpose:
#   Test how Actual and calibrated Situation performance
#   relative to the pre-game closing market relates to
#   subsequent changes in market team strength.
#
#   If official yardage is available from the first calibration,
#   also test the full Actual / Yardage / Situation blend.
#
# Established situational calibration:
#   Situation Points = 6.907 + 0.5632 * Raw Situation
# ============================================================

dir.create("market_calibration_2025", showWarnings = FALSE)

TEAM_FILE <- "calibration_2025/team_game_calibration.csv"

if (!file.exists(TEAM_FILE)) {
  stop(
    "Missing ", TEAM_FILE,
    ". Run the 2025 situational calibration first."
  )
}

tg <- read.csv(TEAM_FILE, stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 1. Load nflverse historical schedules / closing markets
# ------------------------------------------------------------

schedule_url <-
  "https://raw.githubusercontent.com/nflverse/nfldata/master/data/games.csv"

cat("Downloading nflverse schedules and closing lines...\n")

games <- read.csv(
  schedule_url,
  stringsAsFactors = FALSE
)

games <- games[
  games$season == 2025 &
    games$game_type == "REG" &
    games$week >= 1 &
    games$week <= 18,
]

cat("2025 regular-season games:", nrow(games), "\n")

required_market <- c(
  "game_id",
  "week",
  "home_team",
  "away_team",
  "home_score",
  "away_score",
  "spread_line",
  "total_line"
)

missing_market <- setdiff(
  required_market,
  names(games)
)

if (length(missing_market)) {
  stop(
    "Missing schedule columns: ",
    paste(missing_market, collapse = ", ")
  )
}

# Make sure market columns are numeric.
games$spread_line <- suppressWarnings(
  as.numeric(games$spread_line)
)

games$total_line <- suppressWarnings(
  as.numeric(games$total_line)
)

games <- games[
  is.finite(games$spread_line) &
    is.finite(games$total_line),
]

cat("Games with market lines:", nrow(games), "\n")

if (nrow(games) != 272) {
  warning(
    "Expected 272 2025 regular-season games with market lines; found ",
    nrow(games)
  )
}

# nflverse convention:
# positive spread_line means HOME team is favoured.
#
# Therefore:
#
# Home expected points = (total + spread) / 2
# Away expected points = (total - spread) / 2

games$market_home_points <-
  (games$total_line + games$spread_line) / 2

games$market_away_points <-
  (games$total_line - games$spread_line) / 2


# ------------------------------------------------------------
# 2. Convert market data to one row per team-game
# ------------------------------------------------------------

market_rows <- vector(
  "list",
  nrow(games) * 2
)

j <- 1

for (i in seq_len(nrow(games))) {

  g <- games[i, ]

  market_rows[[j]] <- data.frame(
    game_id = g$game_id,
    week = g$week,
    team = g$home_team,
    opponent = g$away_team,
    home = 1,
    market_points = g$market_home_points,
    market_margin = g$spread_line,
    total_line = g$total_line,
    spread_line = g$spread_line,
    stringsAsFactors = FALSE
  )

  j <- j + 1

  market_rows[[j]] <- data.frame(
    game_id = g$game_id,
    week = g$week,
    team = g$away_team,
    opponent = g$home_team,
    home = 0,
    market_points = g$market_away_points,
    market_margin = -g$spread_line,
    total_line = g$total_line,
    spread_line = g$spread_line,
    stringsAsFactors = FALSE
  )

  j <- j + 1
}

market <- do.call(
  rbind,
  market_rows
)


# ------------------------------------------------------------
# 3. Merge our performance data with market expectations
# ------------------------------------------------------------

names(tg)[names(tg) == "posteam"] <- "team"

dat <- merge(
  tg,
  market,
  by = c(
    "game_id",
    "week",
    "team"
  ),
  all = FALSE
)

cat("Matched team-games:", nrow(dat), "\n")

if (nrow(dat) != 544) {
  warning(
    "Expected 544 matched team-games; found ",
    nrow(dat)
  )
}


# ------------------------------------------------------------
# 4. Apply established situational calibration
# ------------------------------------------------------------

dat$situation_points <-
  6.907 + 0.5632 * dat$raw_situation


# ------------------------------------------------------------
# 5. Performance relative to pre-game market
# ------------------------------------------------------------

dat$actual_surprise <-
  dat$actual_points -
  dat$market_points

dat$situation_surprise <-
  dat$situation_points -
  dat$market_points


# Yardage surprise can only be calculated if valid official
# yardage fair points exist in the first calibration.

dat$yardage_surprise <- NA_real_

if ("yardage_fair_points" %in% names(dat)) {

  good_yards <-
    is.finite(dat$yardage_fair_points)

  dat$yardage_surprise[good_yards] <-
    dat$yardage_fair_points[good_yards] -
    dat$market_points[good_yards]
}

cat(
  "Team-games with valid yardage:",
  sum(is.finite(dat$yardage_surprise)),
  "\n"
)


# ------------------------------------------------------------
# 6. Estimate 2025 market home-field advantage
#
# Model:
#
# closing spread =
#     HFA
#   + home team rating
#   - away team rating
#
# The regression intercept therefore represents league HFA.
#
# IMPORTANT:
# We do NOT create a constant 'home = 1' regressor because it
# would be perfectly collinear with the intercept.
# ------------------------------------------------------------

teams <- sort(
  unique(
    c(
      games$home_team,
      games$away_team
    )
  )
)

rating_design <- data.frame(
  margin = games$spread_line,
  stringsAsFactors = FALSE
)

# Use one team as reference to avoid exact collinearity.
reference_team <- teams[1]

for (tm in teams[-1]) {

  rating_design[[tm]] <-
    as.numeric(games$home_team == tm) -
    as.numeric(games$away_team == tm)
}

rating_fit <- lm(
  margin ~ .,
  data = rating_design
)

HFA <- unname(
  coef(rating_fit)["(Intercept)"]
)

if (!is.finite(HFA)) {
  stop(
    "Could not estimate market HFA."
  )
}

cat(
  "Estimated market HFA:",
  round(HFA, 3),
  "\n"
)


# ------------------------------------------------------------
# 7. Estimate market team ratings before each week
#
# For each week W:
#
#   use closing spreads from Weeks < W
#
# and solve:
#
#   spread - HFA =
#       home rating - away rating
#
# with league-average rating constrained to zero.
#
# A small ridge penalty stabilises early-season estimates.
# ------------------------------------------------------------

solve_ratings <- function(
  games_subset,
  teams,
  hfa,
  ridge = 1
) {

  if (nrow(games_subset) < 10) {
    return(NULL)
  }

  A <- matrix(
    0,
    nrow = nrow(games_subset),
    ncol = length(teams)
  )

  colnames(A) <- teams

  for (i in seq_len(nrow(games_subset))) {

    ht <- games_subset$home_team[i]
    at <- games_subset$away_team[i]

    if (
      ht %in% teams &&
      at %in% teams
    ) {

      A[i, ht] <- 1
      A[i, at] <- -1
    }
  }

  y <-
    as.numeric(games_subset$spread_line) -
    hfa

  good <-
    is.finite(y) &
    apply(A, 1, function(z) all(is.finite(z)))

  A <- A[good, , drop = FALSE]
  y <- y[good]

  if (length(y) < 10) {
    return(NULL)
  }

  # Ridge regularisation.
  #
  # This is especially useful early in the season when there
  # are relatively few games from which to infer 32 ratings.

  ridge_matrix <-
    sqrt(ridge) *
    diag(length(teams))

  # Sum-to-zero constraint receives strong weight.

  sum_constraint <-
    matrix(
      1,
      nrow = 1,
      ncol = length(teams)
    ) * 100

  A2 <- rbind(
    A,
    ridge_matrix,
    sum_constraint
  )

  y2 <- c(
    y,
    rep(0, length(teams)),
    0
  )

  fit <- lm.fit(
    x = A2,
    y = y2
  )

  r <- fit$coefficients

  names(r) <- teams

  r
}


# ------------------------------------------------------------
# 8. Create market-rating snapshots
# ------------------------------------------------------------

rating_snapshots <- list()

for (w in 2:19) {

  prior <- games[
    games$week < w,
  ]

  rating_snapshots[[as.character(w)]] <-
    solve_ratings(
      prior,
      teams,
      HFA
    )
}


# ------------------------------------------------------------
# 9. Calculate market revision after each game
#
# Rating before Week W:
#   based on games before W.
#
# Rating before Week W+1:
#   incorporates the Week W result and subsequent closing
#   market information available in our historical sequence.
#
# Revision = next rating - previous rating
# ------------------------------------------------------------

dat$market_rating_pre <- NA_real_
dat$market_rating_next <- NA_real_

for (i in seq_len(nrow(dat))) {

  w <- dat$week[i]
  tm <- dat$team[i]

  if (w >= 2) {

    r_pre <-
      rating_snapshots[[as.character(w)]]

    if (
      !is.null(r_pre) &&
      tm %in% names(r_pre) &&
      is.finite(r_pre[tm])
    ) {

      dat$market_rating_pre[i] <-
        r_pre[tm]
    }
  }

  if (w + 1 <= 19) {

    r_next <-
      rating_snapshots[
        [as.character(w + 1)]
      ]

    if (
      !is.null(r_next) &&
      tm %in% names(r_next) &&
      is.finite(r_next[tm])
    ) {

      dat$market_rating_next[i] <-
        r_next[tm]
    }
  }
}

dat$market_revision <-
  dat$market_rating_next -
  dat$market_rating_pre


# ------------------------------------------------------------
# 10. Core Actual vs Situation analysis
# ------------------------------------------------------------

core <- dat[
  is.finite(dat$market_revision) &
    is.finite(dat$actual_surprise) &
    is.finite(dat$situation_surprise),
]

cat(
  "Core market-revision observations:",
  nrow(core),
  "\n"
)

if (nrow(core) < 100) {
  stop(
    "Too few valid market-revision observations."
  )
}


fit_actual <- lm(
  market_revision ~ actual_surprise,
  data = core
)

fit_situation <- lm(
  market_revision ~ situation_surprise,
  data = core
)

fit_both <- lm(
  market_revision ~
    actual_surprise +
    situation_surprise,
  data = core
)


# ------------------------------------------------------------
# 11. Actual / Situation weight grid
#
# Test every 5 percentage points.
# ------------------------------------------------------------

weights_AS <- data.frame()

for (
  wa in seq(
    0,
    1,
    by = 0.05
  )
) {

  ws <- 1 - wa

  signal <-
    wa * core$actual_surprise +
    ws * core$situation_surprise

  fit <-
    lm(
      core$market_revision ~ signal
    )

  pred <-
    predict(fit)

  rmse <-
    sqrt(
      mean(
        (
          core$market_revision -
            pred
        )^2
      )
    )

  mae <-
    mean(
      abs(
        core$market_revision -
          pred
      )
    )

  correlation <-
    cor(
      signal,
      core$market_revision
    )

  weights_AS <- rbind(
    weights_AS,
    data.frame(
      actual_weight = wa,
      situation_weight = ws,
      rmse = rmse,
      mae = mae,
      correlation = correlation
    )
  )
}

weights_AS <-
  weights_AS[
    order(weights_AS$rmse),
  ]


# ------------------------------------------------------------
# 12. Full three-way weight grid
#
# Runs automatically if valid official yardage exists.
#
# Tests:
# Actual + Yardage + Situation = 100%
#
# in 5% increments.
# ------------------------------------------------------------

weights_AYS <- data.frame()

yard_core <- dat[
  is.finite(dat$market_revision) &
    is.finite(dat$actual_surprise) &
    is.finite(dat$yardage_surprise) &
    is.finite(dat$situation_surprise),
]

cat(
  "Three-way calibration observations:",
  nrow(yard_core),
  "\n"
)

if (nrow(yard_core) > 100) {

  for (
    wa in seq(
      0,
      1,
      by = 0.05
    )
  ) {

    for (
      wy in seq(
        0,
        1 - wa,
        by = 0.05
      )
    ) {

      ws <- 1 - wa - wy

      # Protect against floating point artefacts.
      if (ws < -0.000001) {
        next
      }

      ws <- max(0, ws)

      signal <-
        wa * yard_core$actual_surprise +
        wy * yard_core$yardage_surprise +
        ws * yard_core$situation_surprise

      fit <-
        lm(
          yard_core$market_revision ~
            signal
        )

      pred <-
        predict(fit)

      rmse <-
        sqrt(
          mean(
            (
              yard_core$market_revision -
                pred
            )^2
          )
        )

      mae <-
        mean(
          abs(
            yard_core$market_revision -
              pred
          )
        )

      correlation <-
        cor(
          signal,
          yard_core$market_revision
        )

      weights_AYS <- rbind(
        weights_AYS,
        data.frame(
          actual_weight = wa,
          yardage_weight = wy,
          situation_weight = ws,
          rmse = rmse,
          mae = mae,
          correlation = correlation
        )
      )
    }
  }

  weights_AYS <-
    weights_AYS[
      order(weights_AYS$rmse),
    ]
}


# ------------------------------------------------------------
# 13. Explicit benchmark: current 30 / 30 / 40
# ------------------------------------------------------------

benchmark_text <-
  "30/30/40 benchmark unavailable because official yardage is missing."

if (nrow(yard_core) > 100) {

  benchmark_signal <-
    0.30 * yard_core$actual_surprise +
    0.30 * yard_core$yardage_surprise +
    0.40 * yard_core$situation_surprise

  benchmark_fit <-
    lm(
      yard_core$market_revision ~
        benchmark_signal
    )

  benchmark_pred <-
    predict(benchmark_fit)

  benchmark_rmse <-
    sqrt(
      mean(
        (
          yard_core$market_revision -
            benchmark_pred
        )^2
      )
    )

  benchmark_mae <-
    mean(
      abs(
        yard_core$market_revision -
          benchmark_pred
      )
    )

  benchmark_cor <-
    cor(
      benchmark_signal,
      yard_core$market_revision
    )

  benchmark_text <-
    paste(
      "30/30/40 RMSE:",
      round(benchmark_rmse, 5),
      "| MAE:",
      round(benchmark_mae, 5),
      "| Correlation:",
      round(benchmark_cor, 5)
    )
}


# ------------------------------------------------------------
# 14. Save outputs
# ------------------------------------------------------------

write.csv(
  dat,
  "market_calibration_2025/team_game_market_data.csv",
  row.names = FALSE
)

write.csv(
  core,
  "market_calibration_2025/core_market_revision_data.csv",
  row.names = FALSE
)

write.csv(
  weights_AS,
  "market_calibration_2025/actual_situation_weights.csv",
  row.names = FALSE
)

if (nrow(weights_AYS) > 0) {

  write.csv(
    weights_AYS,
    "market_calibration_2025/three_way_weights.csv",
    row.names = FALSE
  )
}


capture.output(
  summary(fit_actual),
  file =
    "market_calibration_2025/actual_model.txt"
)

capture.output(
  summary(fit_situation),
  file =
    "market_calibration_2025/situation_model.txt"
)

capture.output(
  summary(fit_both),
  file =
    "market_calibration_2025/combined_model.txt"
)


# ------------------------------------------------------------
# 15. Human-readable summary
# ------------------------------------------------------------

summary_lines <- c(

  "NFL NUMBERS — 2025 MARKET CALIBRATION",
  "=====================================",
  "",

  paste(
    "Regular-season games:",
    nrow(games)
  ),

  paste(
    "Matched team-games:",
    nrow(dat)
  ),

  paste(
    "Market-revision observations:",
    nrow(core)
  ),

  paste(
    "Estimated market HFA:",
    round(HFA, 4)
  ),

  "",

  "ACTUAL ONLY",
  paste(
    capture.output(
      coef(fit_actual)
    ),
    collapse = " "
  ),

  paste(
    "R-squared:",
    round(
      summary(fit_actual)$r.squared,
      5
    )
  ),

  "",

  "SITUATION ONLY",
  paste(
    capture.output(
      coef(fit_situation)
    ),
    collapse = " "
  ),

  paste(
    "R-squared:",
    round(
      summary(fit_situation)$r.squared,
      5
    )
  ),

  "",

  "ACTUAL + SITUATION",
  paste(
    capture.output(
      coef(fit_both)
    ),
    collapse = " "
  ),

  paste(
    "R-squared:",
    round(
      summary(fit_both)$r.squared,
      5
    )
  ),

  "",

  "BEST ACTUAL / SITUATION WEIGHTS",
  paste(
    capture.output(
      head(
        weights_AS,
        10
      )
    ),
    collapse = "\n"
  ),

  "",

  "CURRENT 30 / 30 / 40 BENCHMARK",
  benchmark_text
)


if (nrow(weights_AYS) > 0) {

  summary_lines <- c(
    summary_lines,
    "",
    "BEST THREE-WAY WEIGHTS",
    paste(
      capture.output(
        head(
          weights_AYS,
          15
        )
      ),
      collapse = "\n"
    )
  )

} else {

  summary_lines <- c(
    summary_lines,
    "",
    "THREE-WAY TEST NOT RUN",
    paste(
      "Valid official-yardage observations:",
      nrow(yard_core)
    ),
    "We will add the correct official-yardage source separately."
  )
}


writeLines(
  summary_lines,
  "market_calibration_2025/market_summary.txt"
)

cat(
  paste(
    summary_lines,
    collapse = "\n"
  )
)

cat(
  "\n\nSUCCESS — 2025 market calibration complete.\n"
)
