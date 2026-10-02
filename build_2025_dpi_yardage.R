suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(stringr)
})

SEASON <- 2025
DPI_WEIGHT <- 0.50

pbp <- nflreadr::load_pbp(SEASON) |>
  filter(season_type %in% c("REG","POST"))

num0 <- function(x) { x[is.na(x)] <- 0; x }
chr0 <- function(x) { x[is.na(x)] <- ""; x }

required <- c("game_id","season_type","week","posteam","defteam","passing_yards",
              "rushing_yards","sack","yards_gained","penalty","penalty_team",
              "penalty_yards","penalty_type","desc")
missing <- setdiff(required, names(pbp))
if (length(missing)) stop("Missing PBP columns: ", paste(missing, collapse=", "))

pbp <- pbp |>
  mutate(
    desc = chr0(desc),
    penalty_type = chr0(penalty_type),
    penalty_team = chr0(penalty_team),
    penalty = num0(penalty),
    penalty_yards = num0(penalty_yards),
    yards_gained = num0(yards_gained),
    passing_yards = num0(passing_yards),
    rushing_yards = num0(rushing_yards),
    sack = num0(sack),

    # Accepted defensive pass interference only.
    is_dpi = penalty == 1 &
      penalty_yards > 0 &
      penalty_team == defteam &
      (
        str_detect(penalty_type, regex("defensive pass interference|pass interference", ignore_case=TRUE)) |
        str_detect(desc, regex("defensive pass interference|pass interference", ignore_case=TRUE))
      ),

    # Exclude declined / offsetting penalties even if text contains DPI.
    dpi_accepted = is_dpi &
      !str_detect(desc, regex("declined|offsetting|offset penalties|penalties offset", ignore_case=TRUE)),

    dpi_yards = ifelse(dpi_accepted, penalty_yards, 0),
    sack_net_yards = ifelse(sack == 1, pmin(yards_gained, 0), 0)
  )

dpi_audit <- pbp |>
  filter(dpi_accepted) |>
  select(game_id, season_type, week, posteam, defteam, penalty_yards,
         penalty_type, desc)

team_games <- pbp |>
  filter(!is.na(posteam), posteam != "") |>
  group_by(game_id, season_type, week, posteam) |>
  summarise(
    official_net_pass_yards =
      sum(passing_yards, na.rm=TRUE) + sum(sack_net_yards, na.rm=TRUE),
    official_rush_yards = sum(rushing_yards, na.rm=TRUE),
    accepted_defensive_dpi_yards = sum(dpi_yards, na.rm=TRUE),
    dpi_credit = DPI_WEIGHT * accepted_defensive_dpi_yards,
    adjusted_pass_yards = official_net_pass_yards + dpi_credit,
    official_net_offensive_yards = official_net_pass_yards + official_rush_yards,
    adjusted_offensive_yards = adjusted_pass_yards + official_rush_yards,
    .groups="drop"
  ) |>
  arrange(season_type, week, game_id, posteam)

write.csv(team_games, "2025_dpi_adjusted_team_games.csv", row.names=FALSE)
write.csv(dpi_audit, "2025_dpi_audit.csv", row.names=FALSE)

summary_lines <- c(
  "NFL Numbers — 2025 DPI-adjusted yardage",
  "========================================",
  "Scope: 2025 REG + POST",
  "Rule: official net offensive yards + 50% of accepted defensive DPI yards drawn",
  "DPI credit is assigned to passing yardage only",
  "Declined/offsetting DPI excluded",
  "",
  paste("Team-games:", nrow(team_games)),
  paste("Accepted DPI events:", nrow(dpi_audit)),
  paste("Total accepted DPI yards:", sum(team_games$accepted_defensive_dpi_yards)),
  paste("Total model yardage credit:", sum(team_games$dpi_credit))
)
writeLines(summary_lines, "2025_dpi_summary.txt")
cat(paste(summary_lines, collapse="\n"), "\n")
