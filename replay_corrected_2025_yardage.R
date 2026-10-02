suppressPackageStartupMessages({
  library(nflreadr); library(dplyr); library(readr)
})
FLATTEN <- 0.40
DPI_WEIGHT <- 0.50
lr_for <- function(week) ifelse(week==1,0.20,ifelse(week==2,0.175,0.15))
n0 <- function(x){x[is.na(x)]<-0;x}

ratings <- read_csv("pre_week1_2025_yardage_ratings.csv", show_col_types=FALSE)
stopifnot(nrow(ratings)==32, !anyDuplicated(ratings$team))
initial <- ratings

pbp <- nflreadr::load_pbp(2025) |>
  filter(season_type %in% c("REG","POST")) |>
  mutate(
    passing_yards=n0(passing_yards), rushing_yards=n0(rushing_yards),
    yards_gained=n0(yards_gained), sack=n0(sack),
    penalty=n0(penalty), penalty_yards=n0(penalty_yards),
    desc=ifelse(is.na(desc),"",desc),
    penalty_type=ifelse(is.na(penalty_type),"",penalty_type),
    penalty_team=ifelse(is.na(penalty_team),"",penalty_team),
    is_dpi=penalty==1 & penalty_yards>0 & penalty_team==defteam &
      (grepl("pass interference",penalty_type,ignore.case=TRUE) |
       grepl("pass interference",desc,ignore.case=TRUE)) &
      !grepl("declined|offsetting|offset penalties|penalties offset",desc,ignore.case=TRUE),
    dpi_yards=ifelse(is_dpi,penalty_yards,0),
    sack_net=ifelse(sack==1,pmin(yards_gained,0),0)
  )

obs <- pbp |> filter(!is.na(posteam),posteam!="") |>
  group_by(game_id,season_type,week,posteam,defteam) |>
  summarise(net_pass=sum(passing_yards)+sum(sack_net),
            rush=sum(rushing_yards),
            dpi_yards=sum(dpi_yards),
            dpi_credit=DPI_WEIGHT*dpi_yards,
            adj_pass=net_pass+dpi_credit,
            adj_total=adj_pass+rush,.groups="drop")

# Use recovered entering-W1 league baselines, not sample averages.
LG_PASS <- 234
LG_RUSH <- 121

expect_team <- function(team,opp,r){
  t<-r |> filter(.data$team==team); o<-r |> filter(.data$team==opp)
  tibble(exp_pass=t$off_pass*o$def_pass/LG_PASS,
         exp_rush=t$off_rush*o$def_rush/LG_RUSH)
}
calc <- function(x,e,lr){
  raw_share<-ifelse(x$adj_total>0,x$adj_pass/x$adj_total,e$exp_pass/(e$exp_pass+e$exp_rush))
  exp_share<-e$exp_pass/(e$exp_pass+e$exp_rush)
  learned_share<-0.60*raw_share+0.40*exp_share
  lp<-x$adj_total*learned_share; lrush<-x$adj_total-lp
  tibble(raw_pass_share=raw_share,expected_pass_share=exp_share,
         learned_pass_share=learned_share,learned_pass=lp,learned_rush=lrush,
         dpass=lr*(lp-e$exp_pass),drush=lr*(lrush-e$exp_rush))
}

games <- obs |> distinct(game_id,season_type,week) |>
  mutate(type_order=ifelse(season_type=="REG",0,1)) |>
  arrange(type_order,week,game_id)

audit<-list(); snaps<-list()
for(i in seq_len(nrow(games))){
  g<-games[i,]; z<-obs |> filter(game_id==g$game_id)
  if(nrow(z)!=2) stop("Expected two team rows: ",g$game_id)
  a<-z[1,]; b<-z[2,]
  ea<-expect_team(a$posteam,a$defteam,ratings); eb<-expect_team(b$posteam,b$defteam,ratings)
  lr<-lr_for(g$week); ca<-calc(a,ea,lr); cb<-calc(b,eb,lr)
  audit[[length(audit)+1]]<-bind_cols(a |> select(game_id,season_type,week,team=posteam,opponent=defteam,
    net_pass,rush,dpi_yards,dpi_credit,adj_pass,adj_total),ea,ca,learning_rate=lr)
  audit[[length(audit)+1]]<-bind_cols(b |> select(game_id,season_type,week,team=posteam,opponent=defteam,
    net_pass,rush,dpi_yards,dpi_credit,adj_pass,adj_total),eb,cb,learning_rate=lr)

  # simultaneous surprises from the pregame state
  apply_delta<-function(r,team,opp,dp,dr){
    r$off_pass[r$team==team]<-r$off_pass[r$team==team]+dp
    r$off_rush[r$team==team]<-r$off_rush[r$team==team]+dr
    r$def_pass[r$team==opp]<-r$def_pass[r$team==opp]+dp
    r$def_rush[r$team==opp]<-r$def_rush[r$team==opp]+dr
    r
  }
  ratings<-apply_delta(ratings,a$posteam,a$defteam,ca$dpass,ca$drush)
  ratings<-apply_delta(ratings,b$posteam,b$defteam,cb$dpass,cb$drush)
  snaps[[length(snaps)+1]]<-ratings |> mutate(after_game=g$game_id,season_type=g$season_type,week=g$week)
}
audit<-bind_rows(audit); snaps<-bind_rows(snaps)

stopifnot(nrow(audit)==570)
stopifnot(max(abs(audit$learned_pass+audit$learned_rush-audit$adj_total))<1e-8)

write_csv(initial,"recovered_pre_week1_2025_ratings_used.csv")
write_csv(audit,"corrected_2025_yardage_replay_audit.csv")
write_csv(ratings,"corrected_post_2025_yardage_ratings.csv")
write_csv(snaps,"corrected_2025_yardage_rating_snapshots.csv")
writeLines(c(
 "Corrected 2025 Yardage replay",
 "Starting state: exact wk1 ratings recovered from first retained deploy (2025nflEDIT.xlsx).",
 "Entering league baseline: pass 234, rush 121.",
 "No zero/league-average team initialization.",
 "50% accepted defensive DPI credit to passing.",
 "40% regression of observed pass share toward PRE-GAME expected pass share; total yards preserved.",
 "Learning: W1 20%, W2 17.5%, W3+ 15%.",
 "This workflow deliberately STOPS after the 2025 postseason.",
 "It does not invent the 2025->2026 offseason bridge."
),"corrected_2025_yardage_replay_summary.txt")
