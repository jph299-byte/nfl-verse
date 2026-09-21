suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(stringr)
})

# ============================================================
# NFL NUMBERS — 2025 SHORT-YARDAGE SITUATION BACKTEST
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

# Only offensive scrimmage plays with usable down/distance.
q <- pbp |>
  filter(!is.na(posteam), down %in% 1:4, ydstogo > 0)

# ------------------------------------------------------------
# PENALTY TREATMENT
# ------------------------------------------------------------
# nflverse supplies penalty_team / penalty_yards. We use those rather
# than trying to infer the side from prose.
#
# Football gain is separated from enforcement on accepted penalties.
# "No Play" penalty plays do not receive the enforcement distance as
# football gain. Incompletions / pre-snap defensive fouls therefore
# begin from zero football yards.
#
# Defensive penalty-only conversions on 3rd/4th receive minimum
# successful conversion value +1.00 rather than artificial big-play
# credit from the enforcement distance.
# ------------------------------------------------------------

q <- q |>
  mutate(
    accepted_penalty = penalty == 1 & penalty_yards > 0,
    penalty_on_offense = accepted_penalty & penalty_team == posteam,
    penalty_on_defense = accepted_penalty & penalty_team == defteam,
    no_play_text = str_detect(desc, regex("no play", ignore_case=TRUE)),
    incomplete_text = str_detect(desc, regex("incomplete", ignore_case=TRUE)),

    football_gain = yards_gained,

    # nflverse yards_gained on "No Play" can reflect enforcement.
    football_gain = ifelse(accepted_penalty & no_play_text, 0, football_gain),
    football_gain = ifelse(accepted_penalty & incomplete_text & no_play_text, 0, football_gain),

    penalty_effect = case_when(
      penalty_on_offense ~ -PENALTY_WEIGHT * penalty_yards,
      penalty_on_defense ~  PENALTY_WEIGHT * penalty_yards,
      TRUE ~ 0
    ),

    effective_gain = football_gain + penalty_effect,

    # A defensive accepted penalty converts if nflverse marks first down
    # in the text OR its enforcement reaches the line to gain.
    defensive_conversion =
      down %in% c(3,4) &
      penalty_on_defense &
      (
        str_detect(desc, regex("automatic first down|first down", ignore_case=TRUE)) |
        penalty_yards >= ydstogo
      ),

    # Penalty-only means no genuine football gain to reward beyond conversion.
    penalty_only_conversion =
      defensive_conversion &
      (no_play_text | football_gain < ydstogo),

    turnover_event = interception == 1 | fumble == 1
  )

# Interceptions: offensive football gain is zero; retain live penalty effect.
q$football_gain[q$interception == 1] <- 0
q$effective_gain[q$interception == 1] <- q$penalty_effect[q$interception == 1]

# ------------------------------------------------------------
# SITUATION VALUE
# ------------------------------------------------------------

base_value <- function(down, togo, gain, turnover=FALSE, kneel=FALSE) {
  if (kneel) return(0)
  target <- if (down == 1) .4*togo else if (down == 2) .6*togo else togo
  r <- gain/target
  if (down %in% c(3,4) && gain < togo) v <- max(-1.5, r-1)
  else if (r <= 1) v <- max(-1.5, r)
  else v <- min(1.75, 1 + .35*log(r))
  if (turnover) v <- v-1
  v
}

excess_value <- function(down, togo, gain, denom, turnover=FALSE, kneel=FALSE) {
  if (kneel) return(0)

  # First/second down remain exactly as production formula.
  if (!(down %in% c(3,4))) {
    return(base_value(down,togo,gain,turnover,kneel))
  }

  # Failed third/fourth down remains exactly as production formula.
  if (gain < togo) {
    r <- gain/togo
    v <- max(-1.5, r-1)
    if (turnover) v <- v-1
    return(v)
  }

  # Successful third/fourth down:
  # +1 for conversion, then diminishing credit for genuine excess gain.
  excess <- max(0, gain-togo)
  v <- min(1.75, 1 + .35*log(1 + excess/denom))
  if (turnover) v <- v-1
  v
}

score_row <- function(d,t,g,turn,kneel,variant) {
  if (variant == "current") return(base_value(d,t,g,turn,kneel))
  denom <- as.numeric(sub("excess_", "", variant))
  excess_value(d,t,g,denom,turn,kneel)
}

variants <- c("current","excess_4","excess_5","excess_6","excess_7","excess_8")

for (v in variants) {
  vals <- mapply(
    score_row, q$down, q$ydstogo, q$effective_gain,
    q$turnover_event, q$qb_kneel == 1,
    MoreArgs=list(variant=v)
  )

  # Locked new rule: defensive penalty itself converts a failed/no-play
  # third/fourth down => minimum successful conversion value +1.00.
  vals[q$penalty_only_conversion] <- 1.00

  q[[paste0("value_",v)]] <- vals
}

# ------------------------------------------------------------
# TEAM-GAME TABLE
# ------------------------------------------------------------

situation <- q |>
  group_by(game_id, week, posteam) |>
  summarise(
    across(starts_with("value_"), ~sum(.x, na.rm=TRUE)),
    .groups="drop"
  )

# Final scores from the last row of each game.
scores <- pbp |>
  group_by(game_id) |>
  slice_tail(n=1) |>
  transmute(
    game_id, home_team, away_team,
    home_points = total_home_score,
    away_points = total_away_score
  )

# Net offense: nflverse 2025 PBP does not expose a `sack_yards` column.
# `passing_yards` is credited only on completed passes, while sacks are
# represented by negative `yards_gained`. Add those negative sack gains
# to passing + rushing to reproduce net offensive yardage.
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

# Sanity checks.
if (nrow(tg) != 544) warning("Expected 544 team-games; got ", nrow(tg))
if (any(!is.finite(tg$official_offensive_yards))) stop("Bad yardage values")

# ------------------------------------------------------------
# CROSS-VALIDATION
# Fit Situation calibration on opposite parity only.
# Then construct 30/30/40 fair points.
# ------------------------------------------------------------

fold_rows <- list()
pred_rows <- list()

for (v in variants) {
  rawcol <- paste0("value_",v)

  for (train_parity in c("odd","even")) {
    test_parity <- ifelse(train_parity=="odd","even","odd")

    train <- tg[tg$parity == train_parity,]
    test  <- tg[tg$parity == test_parity,]

    fit_data <- data.frame(
      actual_points = train$actual_points,
      raw_situation = train[[rawcol]]
    )
    fit <- lm(actual_points ~ raw_situation, data = fit_data)
    b0 <- unname(coef(fit)[1])
    b1 <- unname(coef(fit)[2])

    situ_pred <- b0 + b1*test[[rawcol]]
    yard_pred <- test$official_offensive_yards / YARDS_PER_POINT
    fair <- .30*test$actual_points + .30*yard_pred + .40*situ_pred

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

    pred_rows[[paste(v,train_parity)]] <- p

    # Situation-only point prediction metrics.
    err <- situ_pred - test$actual_points
    fold_rows[[paste(v,train_parity)]] <- data.frame(
      variant=v,
      train_parity=train_parity,
      test_parity=test_parity,
      intercept=b0,
      slope=b1,
      situation_rmse=sqrt(mean(err^2)),
      situation_mae=mean(abs(err)),
      situation_corr=cor(situ_pred,test$actual_points)
    )
  }
}

pred <- bind_rows(pred_rows)
fold <- bind_rows(fold_rows)

# ------------------------------------------------------------
# DOWNSTREAM GAME-MARGIN METRICS
# Pair team-games into games. Fair margin is posteam minus opponent.
# Compare with actual game margin.
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
    margin_corr=cor(fair_margin,actual_margin),
    .groups="drop"
  ) |>
  arrange(margin_rmse)

# Attach combined Situation metrics.
situ_all <- pred |>
  group_by(variant) |>
  summarise(
    situation_rmse=sqrt(mean((situation_prediction-actual_points)^2)),
    situation_mae=mean(abs(situation_prediction-actual_points)),
    situation_corr=cor(situation_prediction,actual_points),
    .groups="drop"
  )

comparison <- overall |>
  left_join(situ_all, by="variant")

# ------------------------------------------------------------
# SHORT-YARDAGE PLAY BREAKDOWN
# Descriptive only: how much each rule changes successful 3rd/4th plays.
# ------------------------------------------------------------

breakdown_source <- q |>
  filter(down %in% c(3,4)) |>
  mutate(
    distance_band = case_when(
      ydstogo == 1 ~ "1",
      ydstogo <= 3 ~ "2-3",
      ydstogo <= 6 ~ "4-6",
      ydstogo <= 10 ~ "7-10",
      TRUE ~ "11+"
    )
  )

breakdown <- breakdown_source |>
  group_by(distance_band) |>
  summarise(
    plays=n(),
    conversions=sum(effective_gain >= ydstogo | defensive_conversion, na.rm=TRUE),
    penalty_only_conversions=sum(penalty_only_conversion, na.rm=TRUE),
    current=mean(value_current,na.rm=TRUE),
    excess_4=mean(value_excess_4,na.rm=TRUE),
    excess_5=mean(value_excess_5,na.rm=TRUE),
    excess_6=mean(value_excess_6,na.rm=TRUE),
    excess_7=mean(value_excess_7,na.rm=TRUE),
    excess_8=mean(value_excess_8,na.rm=TRUE),
    .groups="drop"
  )

write.csv(comparison, "short_yardage_model_comparison.csv", row.names=FALSE)
write.csv(fold, "short_yardage_fold_results.csv", row.names=FALSE)
write.csv(breakdown, "short_yardage_play_breakdown.csv", row.names=FALSE)

notes <- c(
  "NFL Numbers 2025 short-yardage Situation test",
  "",
  "Variants: current, excess_4, excess_5, excess_6, excess_7, excess_8.",
  "Penalty-only defensive 3rd/4th conversions are fixed at +1.00.",
  "Accepted live penalty yards weighted at 50%.",
  "Each variant is recalibrated on the opposite week parity.",
  "Odd weeks train even; even weeks train odd.",
  "Final fair points = 30% actual + 30% net-yardage/14.5 + 40% calibrated Situation.",
  "",
  "IMPORTANT:",
  "The comparison CSV measures same-game 30/30/40 fair-margin reconstruction.",
  "It does NOT by itself reproduce the later sequential power-rating prediction test.",
  "Use the exported fold predictions as the input for that downstream rating replay.",
  "",
  paste("Team-games:", nrow(tg)),
  paste("Penalty-only conversions:", sum(q$penalty_only_conversion,na.rm=TRUE))
)
writeLines(notes, "short_yardage_test_notes.txt")

print(comparison)
print(fold)
print(breakdown)
