options(stringsAsFactors=FALSE)
suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(stringr)
})

# NFL NUMBERS — SITUATION V3 FULL RAW-PBP REBUILD
# Locked specification through 2026-09-22.
YPP <- 14.5
OUT <- "situation_v3_full"
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)

znum <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
zchr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
flag <- function(x) znum(x) == 1

# Diminishing negative-yardage curve: only actual yards below zero.
neg_value <- function(g) -0.35*log(1 + abs(g)/4)

# Football-play component before event bonuses/possession events.
football_value <- function(d, togo, g) {
  if (!is.finite(d) || !is.finite(togo) || togo <= 0 || !(d %in% 1:4)) return(0)
  if (!is.finite(g)) g <- 0
  if (g < 0) return(neg_value(g))
  if (g == 0) return(0)

  target <- if (d==1) .4*togo else if (d==2) .6*togo else togo

  # Locked /8 treatment: successful 2nd & 1-4, successful 3rd/4th.
  if ((d==2 && togo<=4 && g>=togo) || (d %in% c(3,4) && g>=togo)) {
    excess <- max(0, g-togo)
    return(min(1.75, 1 + .35*log(1 + excess/8)))
  }

  r <- g/target
  if (r <= 1) return(r)
  min(1.75, 1 + .35*log(r))
}

score_one <- function(d,togo,g,desc,penalty,penalty_team,penalty_yards,
                      posteam,defteam,interception,fumble,kneel,spike,
                      touchdown,td_team,safety,punt_attempt,kickoff_attempt,
                      field_goal_attempt,field_goal_result,return_touchdown,
                      two_point_attempt,extra_point_attempt) {
  desc <- zchr(desc)
  dl <- tolower(desc)
  accepted_pen <- penalty==1 && !str_detect(dl,"declined|offsetting|offset")
  no_play <- str_detect(dl,"no play")
  dpi <- accepted_pen && str_detect(dl,"pass interference") &&
         !str_detect(dl,"offensive pass interference") &&
         penalty_team==defteam
  punt <- punt_attempt==1
  kickoff <- kickoff_attempt==1
  fg <- field_goal_attempt==1
  xp <- extra_point_attempt==1
  two <- two_point_attempt==1
  kneel0 <- kneel==1
  spike0 <- spike==1

  # Special-team possession events.
  if (kickoff) {
    v <- 0
    if (return_touchdown==1) v <- v-1
    return(c(v=v, base=0, turnover=0, td_bonus=0, safety_bonus=0,
             penalty_zero=0, dpi=0, fourth_fail=0))
  }
  if (punt) {
    # Nullified/no-play punt penalty: no punt event occurred.
    if (accepted_pen && no_play) v <- 0 else v <- -1
    if (return_touchdown==1 && !(accepted_pen && no_play)) v <- v-1
    return(c(v=v, base=0, turnover=0, td_bonus=0, safety_bonus=0,
             penalty_zero=as.integer(accepted_pen), dpi=0, fourth_fail=0))
  }
  if (fg) {
    # Nullified/no-play kick is zero; otherwise made +1, miss/block -1.
    if (accepted_pen && no_play) v <- 0
    else v <- if (tolower(zchr(field_goal_result))=="made") 1 else -1
    return(c(v=v, base=0, turnover=0, td_bonus=0, safety_bonus=0,
             penalty_zero=as.integer(accepted_pen), dpi=0, fourth_fail=0))
  }

  # PAT/2PT are retained in audit but neutral in Situation. They are neither
  # scrimmage downs nor TDs; no separate value has been specified.
  if (xp || two) {
    return(c(v=0, base=0, turnover=0, td_bonus=0, safety_bonus=0,
             penalty_zero=0, dpi=0, fourth_fail=0))
  }

  if (kneel0 || spike0) {
    return(c(v=0, base=0, turnover=0, td_bonus=0, safety_bonus=0,
             penalty_zero=0, dpi=0, fourth_fail=0))
  }

  # Ordinary accepted penalties contribute no penalty value.
  # If the play is nullified/no-play, whole Situation value is zero.
  if (accepted_pen && !dpi && no_play) {
    # A real scoring safety still survives only if nflverse final safety flag says so.
    sv <- if (safety==1) -1 else 0
    return(c(v=sv, base=0, turnover=0, td_bonus=0,
             safety_bonus=ifelse(safety==1,-1,0),
             penalty_zero=1,dpi=0,fourth_fail=0))
  }

  # DPI: half enforcement distance is the effective football gain.
  effg <- g
  if (dpi) effg <- .5*penalty_yards

  # Interception has zero offensive gain before turnover event.
  if (interception==1) effg <- 0

  base <- football_value(d,togo,effg)
  fourth_fail <- as.integer(d==4 && g < togo && !dpi)

  # 4th down is possession-ending; failed attempt adds -1 to underlying play.
  v <- base - fourth_fail

  # Turnover event: interception OR any fumble, regardless of recovery.
  turn <- as.integer(interception==1 || fumble==1)
  v <- v-turn

  # Offensive TD only. nflverse td_team must equal possession team.
  offensive_td <- as.integer(touchdown==1 && zchr(td_team)==zchr(posteam))
  v <- v + offensive_td

  # Genuine scoring safety: -1 in addition to underlying offensive play.
  sb <- ifelse(safety==1,-1,0)
  v <- v + sb

  c(v=v,base=base,turnover=-turn,td_bonus=offensive_td,safety_bonus=sb,
    penalty_zero=as.integer(accepted_pen && !dpi),dpi=as.integer(dpi),
    fourth_fail=-fourth_fail)
}

prepare <- function(season) {
  cat("Loading ",season," raw nflverse PBP...\n",sep="")
  p <- nflreadr::load_pbp(season) |>
    filter(season_type=="REG")
  if (season==2025) p <- p |> filter(week<=18)

  # Ensure optional columns exist across nflverse schema versions.
  defaults <- list(
    penalty=0, penalty_team="", penalty_yards=0, interception=0, fumble=0,
    qb_kneel=0, qb_spike=0, touchdown=0, td_team="", safety=0,
    punt_attempt=0, kickoff_attempt=0, field_goal_attempt=0,
    field_goal_result="", return_touchdown=0, two_point_attempt=0,
    extra_point_attempt=0, yards_gained=0, desc="", posteam="", defteam="",
    down=NA_real_, ydstogo=NA_real_, passing_yards=0, rushing_yards=0, sack=0
  )
  for (nm in names(defaults)) if (!nm %in% names(p)) p[[nm]] <- defaults[[nm]]

  # Retain all scrimmage downs AND special/scoring plays in chronological ledger.
  keep <- (!is.na(p$down) & p$down %in% 1:4 & zchr(p$posteam)!="") |
          flag(p$punt_attempt) | flag(p$kickoff_attempt) |
          flag(p$field_goal_attempt) | flag(p$extra_point_attempt) |
          flag(p$two_point_attempt) | flag(p$safety)
  q <- p[keep,,drop=FALSE]

  sc <- mapply(score_one,
    q$down,q$ydstogo,znum(q$yards_gained),q$desc,znum(q$penalty),
    q$penalty_team,znum(q$penalty_yards),q$posteam,q$defteam,
    znum(q$interception),znum(q$fumble),znum(q$qb_kneel),znum(q$qb_spike),
    znum(q$touchdown),q$td_team,znum(q$safety),znum(q$punt_attempt),
    znum(q$kickoff_attempt),znum(q$field_goal_attempt),q$field_goal_result,
    znum(q$return_touchdown),znum(q$two_point_attempt),znum(q$extra_point_attempt)
  )
  sc <- t(sc)
  q$situation_v3 <- sc[,"v"]
  q$v3_base <- sc[,"base"]
  q$v3_turnover <- sc[,"turnover"]
  q$v3_td_bonus <- sc[,"td_bonus"]
  q$v3_safety_bonus <- sc[,"safety_bonus"]
  q$v3_penalty_zero <- sc[,"penalty_zero"]
  q$v3_dpi <- sc[,"dpi"]
  q$v3_fourth_fail <- sc[,"fourth_fail"]

  # Team attribution: normal plays to posteam; kickoff/punt/FG to kicking/posteam.
  q$sit_team <- zchr(q$posteam)
  if ("kicker_player_name" %in% names(q)) {} # schema sanity only
  q
}

team_games <- function(p) {
  sit <- p |> filter(sit_team!="") |>
    group_by(game_id,week,sit_team) |>
    summarise(raw_situation=sum(situation_v3,na.rm=TRUE),.groups="drop")

  # Actual final scores.
  scores <- p |> group_by(game_id) |> slice_tail(n=1) |>
    transmute(game_id,home_team,away_team,
              home_points=total_home_score,away_points=total_away_score)

  # Net offensive yards: passing + rushing + negative sack yards.
  y <- p |> filter(posteam!="") |>
    mutate(sack_net=ifelse(znum(sack)==1,pmin(znum(yards_gained),0),0)) |>
    group_by(game_id,week,posteam) |>
    summarise(net_yards=sum(znum(passing_yards))+sum(znum(rushing_yards))+
                         sum(sack_net),.groups="drop")

  sit |> left_join(y,by=c("game_id","week","sit_team"="posteam")) |>
    left_join(scores,by="game_id") |>
    mutate(actual_points=ifelse(sit_team==home_team,home_points,away_points),
           opponent=ifelse(sit_team==home_team,away_team,home_team),
           parity=ifelse(week%%2==1,"odd","even"))
}

p25 <- prepare(2025)
tg25 <- team_games(p25)

fit <- lm(actual_points ~ raw_situation,data=tg25)
cal <- data.frame(intercept=unname(coef(fit)[1]),slope=unname(coef(fit)[2]),
                  team_games=nrow(tg25),
                  raw_mean=mean(tg25$raw_situation),
                  raw_sd=sd(tg25$raw_situation),
                  corr=cor(tg25$raw_situation,tg25$actual_points))
write.csv(cal,file.path(OUT,"situation_v3_calibration_2025.csv"),row.names=FALSE)

# Odd/even interleaved out-of-sample calibration validation.
folds <- list()
preds <- list()
for (trainp in c("odd","even")) {
  testp <- ifelse(trainp=="odd","even","odd")
  tr <- tg25[tg25$parity==trainp,]
  te <- tg25[tg25$parity==testp,]
  f <- lm(actual_points~raw_situation,data=tr)
  pr <- predict(f,newdata=te)
  folds[[trainp]] <- data.frame(
    train=trainp,test=testp,intercept=coef(f)[1],slope=coef(f)[2],
    rmse=sqrt(mean((pr-te$actual_points)^2)),
    mae=mean(abs(pr-te$actual_points)),
    corr=cor(pr,te$actual_points),team_games=nrow(te))
  preds[[trainp]] <- cbind(te,situation_pred=pr)
}
fold <- bind_rows(folds)
write.csv(fold,file.path(OUT,"situation_v3_odd_even_validation.csv"),row.names=FALSE)

# Full 2025 team-game calibrated output.
tg25$situation_fair <- cal$intercept + cal$slope*tg25$raw_situation
tg25$yardage_points <- tg25$net_yards/YPP
tg25$fair_30_30_40 <- .30*tg25$actual_points + .30*tg25$yardage_points +
                      .40*tg25$situation_fair
write.csv(tg25,file.path(OUT,"situation_v3_2025_team_games.csv"),row.names=FALSE)

# 2026 W1/W2 under the new 2025 calibration.
p26 <- prepare(2026)

# Hard fail if any missed/blocked FG is not -1 or made FG is not +1.
fg_check <- p26 |> filter(znum(field_goal_attempt)==1)
bad_fg <- fg_check |>
  filter((tolower(zchr(field_goal_result))=="made" & situation_v3 != 1) |
         (tolower(zchr(field_goal_result))!="made" & situation_v3 != -1))
if (nrow(bad_fg)) stop("FIELD-GOAL SANITY CHECK FAILED: ", nrow(bad_fg), " bad rows")

tg26 <- team_games(p26)
tg26$situation_fair <- cal$intercept + cal$slope*tg26$raw_situation
tg26$yardage_points <- tg26$net_yards/YPP
tg26$fair_30_30_40_raw <- .30*tg26$actual_points + .30*tg26$yardage_points +
                          .40*tg26$situation_fair
write.csv(tg26,file.path(OUT,"situation_v3_2026_weeks1_2_team_games.csv"),row.names=FALSE)

# Complete chronological audit ledgers.
audit_cols <- intersect(c("game_id","week","play_id","qtr","game_seconds_remaining",
  "posteam","defteam","down","ydstogo","yards_gained","desc","penalty",
  "penalty_team","penalty_yards","interception","fumble","touchdown","td_team",
  "safety","punt_attempt","kickoff_attempt","field_goal_attempt",
  "field_goal_result","return_touchdown","two_point_attempt","extra_point_attempt",
  "situation_v3","v3_base","v3_turnover","v3_td_bonus","v3_safety_bonus",
  "v3_penalty_zero","v3_dpi","v3_fourth_fail"),names(p26))
write.csv(p26[,audit_cols],file.path(OUT,"situation_v3_2026_complete_pbp.csv"),row.names=FALSE)

car <- p26 |> filter(week==2, game_id %in% unique(game_id[
  (home_team=="CAR" & away_team=="ATL") | (home_team=="ATL" & away_team=="CAR")
]))
write.csv(car[,intersect(audit_cols,names(car))],
          file.path(OUT,"CAR_ATL_week2_full_audit.csv"),row.names=FALSE)

# Edge-case counts make silent extraction failures obvious.
edge <- data.frame(
  metric=c("2025 plays retained","2025 punts","2025 kickoffs","2025 FGs",
           "2025 DPI","2025 safeties","2025 offensive TD bonuses",
           "2026 W1-2 plays retained","2026 W1-2 punts","2026 W1-2 kickoffs",
           "2026 W1-2 FGs","2026 W1-2 safeties"),
  count=c(nrow(p25),sum(flag(p25$punt_attempt)),sum(flag(p25$kickoff_attempt)),
          sum(flag(p25$field_goal_attempt)),sum(p25$v3_dpi),
          sum(flag(p25$safety)),sum(p25$v3_td_bonus),
          nrow(p26),sum(flag(p26$punt_attempt)),sum(flag(p26$kickoff_attempt)),
          sum(flag(p26$field_goal_attempt)),sum(flag(p26$safety)))
)
write.csv(edge,file.path(OUT,"extraction_sanity_counts.csv"),row.names=FALSE)

cat("\nFULL SITUATION V3 REBUILD COMPLETE\n")
print(cal)
cat("\nOdd/even validation:\n"); print(fold)
cat("\nExtraction counts:\n"); print(edge)
