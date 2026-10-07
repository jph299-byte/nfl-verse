suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(stringr)
})

# ============================================================
# NFL NUMBERS — 2025 SECOND-DOWN TARGET A/B BACKTEST
#
# A: CURRENT
#    1st = 40% of yards to go
#    2nd = 60% of yards to go
#    3rd/4th = full distance
#
# B: PROPOSED
#    1st = 40% of yards to go
#    2nd & 1-4 = full distance
#    2nd & 5+ = 70% of yards to go
#    3rd/4th = full distance
#
# Everything else is held constant.
# Test only. Does not modify production files.
# ============================================================

PENALTY_WEIGHT <- 0.50
YARDS_PER_POINT <- 14.5

pbp <- nflreadr::load_pbp(2025) |>
  filter(season_type == "REG", week <= 18)

need <- c("game_id","week","posteam","defteam","down","ydstogo","yards_gained",
          "desc","penalty","penalty_team","penalty_yards","penalty_type",
          "interception","fumble","qb_kneel","touchdown","total_home_score",
          "total_away_score","home_team","away_team","passing_yards",
          "rushing_yards","sack")
missing <- setdiff(need, names(pbp))
if (length(missing)) stop("Missing PBP columns: ", paste(missing, collapse=", "))

num0 <- function(x) { x[is.na(x)] <- 0; x }
chr0 <- function(x) { x[is.na(x)] <- ""; x }

pbp <- pbp |>
  mutate(
    desc = chr0(desc),
    penalty_type = chr0(penalty_type),
    penalty_team = chr0(penalty_team),
    penalty = num0(penalty),
    penalty_yards = num0(penalty_yards),
    interception = num0(interception),
    fumble = num0(fumble),
    qb_kneel = num0(qb_kneel),
    yards_gained = num0(yards_gained)
  )

q <- pbp |>
  filter(!is.na(posteam), down %in% 1:4, ydstogo > 0) |>
  mutate(
    accepted_penalty = penalty == 1 & penalty_yards > 0,
    penalty_on_offense = accepted_penalty & penalty_team == posteam,
    penalty_on_defense = accepted_penalty & penalty_team == defteam,
    no_play_text = str_detect(desc, regex("no play", ignore_case=TRUE)),
    incomplete_text = str_detect(desc, regex("incomplete", ignore_case=TRUE)),
    football_gain = yards_gained,
    football_gain = ifelse(accepted_penalty & no_play_text, 0, football_gain),
    football_gain = ifelse(accepted_penalty & incomplete_text & no_play_text, 0, football_gain),
    penalty_effect = case_when(
      penalty_on_offense ~ -PENALTY_WEIGHT * penalty_yards,
      penalty_on_defense ~  PENALTY_WEIGHT * penalty_yards,
      TRUE ~ 0
    ),
    effective_gain = football_gain + penalty_effect,
    defensive_conversion =
      down %in% c(3,4) &
      penalty_on_defense &
      (
        str_detect(desc, regex("automatic first down|first down", ignore_case=TRUE)) |
        penalty_yards >= ydstogo
      ),
    penalty_only_conversion =
      defensive_conversion &
      (no_play_text | football_gain < ydstogo),
    turnover_event = interception == 1 | fumble == 1
  )

q$football_gain[q$interception == 1] <- 0
q$effective_gain[q$interception == 1] <- q$penalty_effect[q$interception == 1]

# ------------------------------------------------------------
# SITUATION VALUE
# Only target selection differs between variants.
# ------------------------------------------------------------

situation_value <- function(down, togo, gain, variant,
                            turnover=FALSE, kneel=FALSE) {
  if (kneel) return(0)

  if (down == 1) {
    target <- .4 * togo
  } else if (down == 2) {
    if (variant == "current_60") {
      target <- .6 * togo
    } else if (variant == "proposed_100_70") {
      target <- if (togo <= 4) togo else .7 * togo
    } else {
      stop("Unknown variant: ", variant)
    }
  } else {
    target <- togo
  }

  r <- gain / target

  if (down %in% c(3,4) && gain < togo) {
    v <- max(-1.5, r - 1)
  } else if (r <= 1) {
    v <- max(-1.5, r)
  } else {
    v <- min(1.75, 1 + .35 * log(r))
  }

  if (turnover) v <- v - 1
  v
}

variants <- c("current_60", "proposed_100_70")

for (v in variants) {
  vals <- mapply(
    situation_value,
    q$down, q$ydstogo, q$effective_gain,
    q$turnover_event, q$qb_kneel == 1,
    MoreArgs=list(variant=v)
  )

  # Preserve existing penalty-only 3rd/4th conversion treatment.
  vals[q$penalty_only_conversion] <- 1.00
  q[[paste0("value_", v)]] <- vals
}

# ------------------------------------------------------------
# PLAY-LEVEL IMPACT OF THE NEW SECOND-DOWN RULE
# ------------------------------------------------------------

second_down_impact <- q |>
  filter(down == 2) |>
  mutate(
    target_current = .6 * ydstogo,
    target_proposed = ifelse(ydstogo <= 4, ydstogo, .7 * ydstogo),
    value_change = value_proposed_100_70 - value_current_60,
    distance_band = case_when(
      ydstogo == 1 ~ "2nd & 1",
      ydstogo == 2 ~ "2nd & 2",
      ydstogo == 3 ~ "2nd & 3",
      ydstogo == 4 ~ "2nd & 4",
      ydstogo == 5 ~ "2nd & 5",
      ydstogo == 6 ~ "2nd & 6",
      ydstogo <= 10 ~ "2nd & 7-10",
      TRUE ~ "2nd & 11+"
    )
  ) |>
  group_by(distance_band) |>
  summarise(
    plays = n(),
    avg_current = mean(value_current_60, na.rm=TRUE),
    avg_proposed = mean(value_proposed_100_70, na.rm=TRUE),
    avg_change = mean(value_change, na.rm=TRUE),
    total_change = sum(value_change, na.rm=TRUE),
    .groups="drop"
  )

# ------------------------------------------------------------
# TEAM-GAME TABLE
# ------------------------------------------------------------

situation <- q |>
  group_by(game_id, week, posteam) |>
  summarise(
    across(starts_with("value_"), ~sum(.x, na.rm=TRUE)),
    .groups="drop"
  )

scores <- pbp |>
  group_by(game_id) |>
  slice_tail(n=1) |>
  transmute(
    game_id, home_team, away_team,
    home_points = total_home_score,
    away_points = total_away_score
  )

yards <- pbp |>
  filter(!is.na(posteam)) |>
  mutate(
    sack_net_yards = ifelse(
      coalesce(sack, 0) == 1,
      pmin(num0(yards_gained), 0),
      0
    )
  ) |>
  group_by(game_id, week, posteam) |>
  summarise(
    official_offensive_yards =
      sum(num0(passing_yards), na.rm=TRUE) +
      sum(num0(rushing_yards), na.rm=TRUE) +
      sum(sack_net_yards, na.rm=TRUE),
    .groups="drop"
  )

tg <- situation |>
  left_join(yards, by=c("game_id","week","posteam")) |>
  left_join(scores, by="game_id") |>
  mutate(
    actual_points = ifelse(posteam == home_team, home_points, away_points),
    opponent = ifelse(posteam == home_team, away_team, home_team),
    parity = ifelse(week %% 2 == 1, "odd", "even")
  )

if (nrow(tg) != 544) warning("Expected 544 REG team-games; got ", nrow(tg))
if (any(!is.finite(tg$official_offensive_yards))) stop("Bad yardage values")

# ------------------------------------------------------------
# OPPOSITE-PARITY CROSS-VALIDATION
# Each variant receives its own calibration.
# ------------------------------------------------------------

fold_rows <- list()
pred_rows <- list()

for (v in variants) {
  rawcol <- paste0("value_", v)

  for (train_parity in c("odd","even")) {
    test_parity <- ifelse(train_parity == "odd", "even", "odd")

    train <- tg[tg$parity == train_parity,]
    test  <- tg[tg$parity == test_parity,]

    fit <- lm(
      actual_points ~ raw_situation,
      data=data.frame(
        actual_points=train$actual_points,
        raw_situation=train[[rawcol]]
      )
    )

    b0 <- unname(coef(fit)[1])
    b1 <- unname(coef(fit)[2])

    situ_pred <- b0 + b1 * test[[rawcol]]
    yard_pred <- test$official_offensive_yards / YARDS_PER_POINT
    fair <- .30 * test$actual_points + .30 * yard_pred + .40 * situ_pred

    p <- test |>
      transmute(
        game_id, week, posteam, opponent,
        variant=v,
        train_parity=train_parity,
        test_parity=test_parity,
        actual_points,
        raw_situation=.data[[rawcol]],
        situation_prediction=situ_pred,
        yardage_prediction=yard_pred,
        fair_points=fair
      )

    pred_rows[[paste(v, train_parity)]] <- p

    err <- situ_pred - test$actual_points
    fold_rows[[paste(v, train_parity)]] <- data.frame(
      variant=v,
      train_parity=train_parity,
      test_parity=test_parity,
      intercept=b0,
      slope=b1,
      situation_rmse=sqrt(mean(err^2)),
      situation_mae=mean(abs(err)),
      situation_corr=cor(situ_pred, test$actual_points)
    )
  }
}

pred <- bind_rows(pred_rows)
fold <- bind_rows(fold_rows)

# ------------------------------------------------------------
# SAME-GAME FAIR-MARGIN METRICS
# Useful diagnostic, but sequential replay below is the more important
# test for whether this improves the rating system.
# ------------------------------------------------------------

game_preds <- pred |>
  inner_join(
    pred |>
      select(game_id, variant, train_parity, posteam,
             opp_actual=actual_points, opp_fair=fair_points),
    by=c("game_id","variant","train_parity","opponent"="posteam")
  ) |>
  filter(posteam < opponent) |>
  mutate(
    actual_margin = actual_points - opp_actual,
    fair_margin = fair_points - opp_fair,
    error = fair_margin - actual_margin
  )

overall <- game_preds |>
  group_by(variant) |>
  summarise(
    games=n(),
    margin_rmse=sqrt(mean(error^2)),
    margin_mae=mean(abs(error)),
    margin_corr=cor(fair_margin, actual_margin),
    .groups="drop"
  )

situ_all <- pred |>
  group_by(variant) |>
  summarise(
    situation_rmse=sqrt(mean((situation_prediction-actual_points)^2)),
    situation_mae=mean(abs(situation_prediction-actual_points)),
    situation_corr=cor(situation_prediction, actual_points),
    .groups="drop"
  )

comparison <- overall |>
  left_join(situ_all, by="variant") |>
  arrange(margin_rmse)

# ------------------------------------------------------------
# SEQUENTIAL ZERO-SUM POWER-RATING REPLAY
# Controlled neutral starting ratings, exactly as prior test:
# W1 20%; W2 17.5%; W3+ 15%.
# ------------------------------------------------------------

learning_rate <- function(w) {
  if (w == 1) return(.20)
  if (w == 2) return(.175)
  .15
}

run_replay <- function(v) {
  x <- pred |>
    filter(variant == v) |>
    arrange(week, game_id)

  teams <- sort(unique(c(x$posteam, x$opponent)))
  rating <- setNames(rep(0, length(teams)), teams)
  out <- list()
  k <- 1

  for (w in sort(unique(x$week))) {
    wx <- x |> filter(week == w)

    games <- wx |>
      inner_join(
        wx |>
          select(game_id, posteam,
                 opp_actual=actual_points,
                 opp_fair=fair_points),
        by=c("game_id","opponent"="posteam")
      ) |>
      filter(posteam < opponent)

    week_updates <- setNames(rep(0, length(teams)), teams)

    for (i in seq_len(nrow(games))) {
      a <- games$posteam[i]
      b <- games$opponent[i]

      pred_margin <- rating[[a]] - rating[[b]]
      actual_margin <- games$actual_points[i] - games$opp_actual[i]
      fair_margin <- games$fair_points[i] - games$opp_fair[i]

      surprise <- fair_margin - pred_margin
      move <- learning_rate(w) * surprise

      week_updates[[a]] <- week_updates[[a]] + move
      week_updates[[b]] <- week_updates[[b]] - move

      out[[k]] <- data.frame(
        variant=v,
        week=w,
        game_id=games$game_id[i],
        team_a=a,
        team_b=b,
        pregame_rating_a=rating[[a]],
        pregame_rating_b=rating[[b]],
        predicted_margin=pred_margin,
        actual_margin=actual_margin,
        fair_margin=fair_margin,
        surprise=surprise,
        movement=move,
        error=pred_margin-actual_margin
      )
      k <- k + 1
    }

    rating <- rating + week_updates
  }

  bind_rows(out)
}

replay <- bind_rows(lapply(variants, run_replay))

replay_summary <- replay |>
  filter(week >= 2) |>
  group_by(variant) |>
  summarise(
    games=n(),
    rmse=sqrt(mean(error^2)),
    mae=mean(abs(error)),
    corr=cor(predicted_margin, actual_margin),
    .groups="drop"
  ) |>
  arrange(rmse)

replay_windows <- bind_rows(
  lapply(c(2,3,5,9), function(start_week) {
    replay |>
      filter(week >= start_week) |>
      group_by(variant) |>
      summarise(
        start_week=start_week,
        games=n(),
        rmse=sqrt(mean(error^2)),
        mae=mean(abs(error)),
        corr=cor(predicted_margin, actual_margin),
        .groups="drop"
      )
  })
) |>
  arrange(start_week, rmse)

# Direct A/B differences: negative means proposed is better.
ab_summary <- replay_summary |>
  select(variant, rmse, mae, corr) |>
  tidyr::pivot_wider(
    names_from=variant,
    values_from=c(rmse,mae,corr)
  ) |>
  transmute(
    rmse_change = rmse_proposed_100_70 - rmse_current_60,
    mae_change = mae_proposed_100_70 - mae_current_60,
    corr_change = corr_proposed_100_70 - corr_current_60
  )

# ------------------------------------------------------------
# OUTPUT
# ------------------------------------------------------------

write.csv(comparison, "second_down_target_model_comparison.csv", row.names=FALSE)
write.csv(fold, "second_down_target_fold_results.csv", row.names=FALSE)
write.csv(second_down_impact, "second_down_target_play_impact.csv", row.names=FALSE)
write.csv(replay, "second_down_target_sequential_predictions.csv", row.names=FALSE)
write.csv(replay_summary, "second_down_target_sequential_summary.csv", row.names=FALSE)
write.csv(replay_windows, "second_down_target_sequential_windows.csv", row.names=FALSE)
write.csv(ab_summary, "second_down_target_AB_summary.csv", row.names=FALSE)

cat("\n============================================================\n")
cat("2025 SECOND-DOWN TARGET A/B TEST\n")
cat("============================================================\n\n")

cat("SAME-GAME / CALIBRATION DIAGNOSTICS\n")
print(comparison)

cat("\nSEQUENTIAL POWER-RATING REPLAY — W2+\n")
print(replay_summary)

cat("\nSEQUENTIAL WINDOWS\n")
print(replay_windows)

cat("\nDIRECT PROPOSED MINUS CURRENT\n")
print(ab_summary)
cat("\nNegative RMSE/MAE change = improvement. Positive corr change = improvement.\n")

cat("\nSECOND-DOWN PLAY IMPACT\n")
print(second_down_impact)
