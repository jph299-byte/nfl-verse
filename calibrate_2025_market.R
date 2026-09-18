options(stringsAsFactors = FALSE)

# ============================================================
# NFL NUMBERS — 2025 MARKET-BASED WEIGHT CALIBRATION
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

games <- read.csv(schedule_url, stringsAsFactors = FALSE)

games <- games[
  games$season == 2025 &
  games$game_type == "REG" &
  games$week >= 1 &
  games$week <= 18,
]

cat("2025 regular-season games:", nrow(games), "\n")

required_market <- c(
  "game_id", "week",
  "home_team", "away_team",
  "home_score", "away_score",
  "spread_line", "total_line"
)

missing_market <- setdiff(required_market, names(games))

if (length(missing_market)) {
  stop(
    "Missing schedule columns: ",
    paste(missing_market, collapse = ", ")
  )
}

games <- games[
  is.finite(games$spread_line) &
  is.finite(games$total_line),
]

cat("Games with market lines:", nrow(games), "\n")

# nflverse spread_line convention:
# positive = HOME team favoured.

games$market_home_margin <- games$spread_line

games$market_home_points <-
  (games$total_line + games$spread_line) / 2

games$market_away_points <-
  (games$total_line - games$spread_line) / 2

# ------------------------------------------------------------
# 2. Convert games to team-game market rows
# ------------------------------------------------------------

market_rows <- list()

for (i in seq_len(nrow(games))) {

  g <- games[i, ]

  market_rows[[length(market_rows) + 1]] <-
    data.frame(
      game_id = g$game_id,
      week = g$week,
      team = g$home_team,
      opponent = g$away_team,
      home = 1,
      market_points = g$market_home_points,
      market_margin = g$spread_line,
      total_line = g$total_line,
      spread_line = g$spread_line
    )

  market_rows[[length(market_rows) + 1]] <-
    data.frame(
      game_id = g$game_id,
      week = g$week,
      team = g$away_team,
      opponent = g$home_team,
      home = 0,
      market_points = g$market_away_points,
      market_margin = -g$spread_line,
      total_line = g$total_line,
      spread_line = g$spread_line
    )
}

market <- do.call(rbind, market_rows)

# ------------------------------------------------------------
# 3. Merge our performance model
# ------------------------------------------------------------

names(tg)[names(tg) == "posteam"] <- "team"

dat <- merge(
  tg,
  market,
  by = c("game_id", "week", "team"),
  all = FALSE
)

cat("Matched team-games:", nrow(dat), "\n")

if (nrow(dat) < 500) {
  warning("Unexpectedly low team-game match count.")
}

# Established 2025 calibration:
# Situation Points = 6.907 + 0.5632 * Raw

dat$situation_points <-
  6.907 + 0.5632 * dat$raw_situation

# ------------------------------------------------------------
# 4. Build performance SURPRISES relative to market
# ------------------------------------------------------------

dat$actual_surprise <-
  dat$actual_points - dat$market_points

# Yardage is used only where the first calibration actually
# supplied valid official net offensive yards.

dat$yardage_surprise <- NA_real_

if ("yardage_fair_points" %in% names(dat)) {
  good_yards <- is.finite(dat$yardage_fair_points)

  dat$yardage_surprise[good_yards] <-
    dat$yardage_fair_points[good_yards] -
    dat$market_points[good_yards]
}

dat$situation_surprise <-
  dat$situation_points - dat$market_points

# ------------------------------------------------------------
# 5. Estimate market team ratings from closing spreads
#
# spread = home_rating - away_rating + HFA
#
# Estimate HFA and one rating for each team using all 2025
# closing spreads.
# ------------------------------------------------------------

teams <- sort(unique(c(games$home_team, games$away_team)))

rating_design <- data.frame(
  margin = games$spread_line,
  home = 1,
  stringsAsFactors = FALSE
)

for (tm in teams[-1]) {
  rating_design[[tm]] <-
    as.numeric(games$home_team == tm) -
    as.numeric(games$away_team == tm)
}

rating_fit <- lm(
  margin ~ .,
  data = rating_design
)

HFA <- unname(coef(rating_fit)["home"])

cat("Estimated market HFA:", round(HFA, 3), "\n")

# ------------------------------------------------------------
# 6. More useful target:
# game-to-game change in market assessment.
#
# Use each team's market margin after removing home/away HFA.
# This is still opponent-relative, so create a schedule-adjusted
# rolling market rating using ridge-style least squares week by
# week.
# ------------------------------------------------------------

solve_ratings <- function(games_subset, teams, hfa) {

  if (nrow(games_subset) < 10) return(NULL)

  A <- matrix(
    0,
    nrow = nrow(games_subset),
    ncol = length(teams)
  )

  colnames(A) <- teams

  for (i in seq_len(nrow(games_subset))) {
    A[i, games_subset$home_team[i]] <- 1
    A[i, games_subset$away_team[i]] <- -1
  }

  y <- games_subset$spread_line - hfa

  # Identifiability constraint: league ratings sum to zero.
  A2 <- rbind(A, rep(1, length(teams)))
  y2 <- c(y, 0)

  fit <- lm.fit(A2, y2)

  r <- fit$coefficients
  names(r) <- teams
  r
}

# Market rating available BEFORE each week:
# use all closing lines up through previous week.
#
# Early weeks do not have enough current-season information,
# so begin measuring market revisions from Week 3.

rating_snapshots <- list()

for (w in 2:18) {

  prior <- games[games$week < w, ]

  rating_snapshots[[as.character(w)]] <-
    solve_ratings(prior, teams, HFA)
}

dat$market_rating_pre <- NA_real_
dat$market_rating_next <- NA_real_

for (i in seq_len(nrow(dat))) {

  w <- dat$week[i]
  tm <- dat$team[i]

  if (w >= 2) {
    r1 <- rating_snapshots[[as.character(w)]]
    if (!is.null(r1) && tm %in% names(r1)) {
      dat$market_rating_pre[i] <- r1[tm]
    }
  }

  if (w + 1 <= 18) {
    r2 <- rating_snapshots[[as.character(w + 1)]]
    if (!is.null(r2) && tm %in% names(r2)) {
      dat$market_rating_next[i] <- r2[tm]
    }
  }
}

dat$market_revision <-
  dat$market_rating_next - dat$market_rating_pre

# ------------------------------------------------------------
# 7. Core analysis WITHOUT yardage
#
# This can run immediately even if official yards are absent.
# ------------------------------------------------------------

core <- dat[
  is.finite(dat$market_revision) &
  is.finite(dat$actual_surprise) &
  is.finite(dat$situation_surprise),
]

cat("Core market-revision observations:", nrow(core), "\n")

fit_actual <- lm(
  market_revision ~ actual_surprise,
  data = core
)

fit_situation <- lm(
  market_revision ~ situation_surprise,
  data = core
)

fit_both <- lm(
  market_revision ~ actual_surprise + situation_surprise,
  data = core
)

# ------------------------------------------------------------
# 8. Weight grid: Actual vs Situation
# ------------------------------------------------------------

weights_AS <- data.frame()

for (wa in seq(0, 1, by = .05)) {

  ws <- 1 - wa

  signal <-
    wa * core$actual_surprise +
    ws * core$situation_surprise

  fit <- lm(core$market_revision ~ signal)

  pred <- predict(fit)

  rmse <- sqrt(mean(
    (core$market_revision - pred)^2
  ))

  weights_AS <- rbind(
    weights_AS,
    data.frame(
      actual_weight = wa,
      situation_weight = ws,
      rmse = rmse,
      correlation = cor(
        signal,
        core$market_revision
      )
    )
  )
}

weights_AS <- weights_AS[
  order(weights_AS$rmse),
]

# ------------------------------------------------------------
# 9. Three-way grid if official yardage exists
# ------------------------------------------------------------

weights_AYS <- data.frame()

yard_core <- dat[
  is.finite(dat$market_revision) &
  is.finite(dat$actual_surprise) &
  is.finite(dat$yardage_surprise) &
  is.finite(dat$situation_surprise),
]

if (nrow(yard_core) > 100) {

  for (wa in seq(0, 1, by = .05)) {
    for (wy in seq(0, 1 - wa, by = .05)) {

      ws <- 1 - wa - wy

      signal <-
        wa * yard_core$actual_surprise +
        wy * yard_core$yardage_surprise +
        ws * yard_core$situation_surprise

      fit <- lm(
        yard_core$market_revision ~ signal
      )

      pred <- predict(fit)

      rmse <- sqrt(mean(
        (yard_core$market_revision - pred)^2
      ))

      weights_AYS <- rbind(
        weights_AYS,
        data.frame(
          actual_weight = wa,
          yardage_weight = wy,
          situation_weight = ws,
          rmse = rmse,
          correlation = cor(
            signal,
            yard_core$market_revision
          )
        )
      )
    }
  }

  weights_AYS <- weights_AYS[
    order(weights_AYS$rmse),
  ]
}

# ------------------------------------------------------------
# 10. Save everything
# ------------------------------------------------------------

write.csv(
  dat,
  "market_calibration_2025/team_game_market_data.csv",
  row.names = FALSE
)

write.csv(
  weights_AS,
  "market_calibration_2025/actual_situation_weights.csv",
  row.names = FALSE
)

if (nrow(weights_AYS)) {
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

summary_lines <- c(
  "NFL NUMBERS — 2025 MARKET CALIBRATION",
  "=====================================",
  "",
  paste("Games with closing market:", nrow(games)),
  paste("Matched team-games:", nrow(dat)),
  paste(
    "Market-revision observations:",
    nrow(core)
  ),
  paste(
    "Estimated market HFA:",
    round(HFA, 3)
  ),
  "",
  "ACTUAL ONLY",
  paste(
    capture.output(coef(fit_actual)),
    collapse = " "
  ),
  paste(
    "R-squared:",
    round(summary(fit_actual)$r.squared, 4)
  ),
  "",
  "SITUATION ONLY",
  paste(
    capture.output(coef(fit_situation)),
    collapse = " "
  ),
  paste(
    "R-squared:",
    round(summary(fit_situation)$r.squared, 4)
  ),
  "",
  "ACTUAL + SITUATION",
  paste(
    capture.output(coef(fit_both)),
    collapse = " "
  ),
  paste(
    "R-squared:",
    round(summary(fit_both)$r.squared, 4)
  ),
  "",
  "BEST ACTUAL/SITUATION WEIGHT GRID",
  paste(
    capture.output(head(weights_AS, 10)),
    collapse = "\n"
  )
)

if (nrow(weights_AYS)) {

  summary_lines <- c(
    summary_lines,
    "",
    "BEST THREE-WAY WEIGHTS",
    paste(
      capture.output(head(weights_AYS, 15)),
      collapse = "\n"
    )
  )

} else {

  summary_lines <- c(
    summary_lines,
    "",
    "THREE-WAY TEST NOT RUN:",
    "Official yardage was unavailable in the first calibration."
  )
}

writeLines(
  summary_lines,
  "market_calibration_2025/market_summary.txt"
)

cat(paste(summary_lines, collapse = "\n"))
cat("\n\nSUCCESS — market calibration complete.\n")
