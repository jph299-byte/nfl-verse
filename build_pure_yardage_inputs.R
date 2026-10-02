suppressPackageStartupMessages({
  library(nflreadr)
  library(dplyr)
  library(stringr)
})

DPI_WEIGHT <- 0.50
SEASONS <- c(2025, 2026)
MAX_2026_WEEK <- 3L

num0 <- function(x) { x[is.na(x)] <- 0; x }
chr0 <- function(x) { x[is.na(x)] <- ""; x }

pbp <- nflreadr::load_pbp(SEASONS) |>
  filter(
    (season == 2025 & season_type %in% c("REG", "POST")) |
    (season == 2026 & season_type == "REG" & week <= MAX_2026_WEEK)
  )

required <- c(
  "game_id","season","season_type","week","posteam","defteam",
  "passing_yards","rushing_yards","sack","yards_gained",
  "penalty","penalty_team","penalty_yards","penalty_type","desc"
)
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

    is_dpi = penalty == 1 &
      penalty_yards > 0 &
      penalty_team == defteam &
      (
        str_detect(penalty_type, regex("defensive pass interference|pass interference", ignore_case=TRUE)) |
        str_detect(desc, regex("defensive pass interference|pass interference", ignore_case=TRUE))
      ),

    dpi_accepted = is_dpi &
      !str_detect(desc, regex("declined|offsetting|offset penalties|penalties offset", ignore_case=TRUE)),

    dpi_yards = ifelse(dpi_accepted, penalty_yards, 0),
    sack_net_yards = ifelse(sack == 1, pmin(yards_gained, 0), 0)
  )

dpi_audit <- pbp |>
  filter(dpi_accepted) |>
  select(season, game_id, season_type, week, posteam, defteam,
         penalty_yards, penalty_type, desc) |>
  arrange(season, season_type, week, game_id, posteam)

team_games <- pbp |>
  filter(!is.na(posteam), posteam != "") |>
  group_by(season, game_id, season_type, week, posteam) |>
  summarise(
    official_net_pass_yards =
      sum(passing_yards, na.rm=TRUE) + sum(sack_net_yards, na.rm=TRUE),
    official_rush_yards = sum(rushing_yards, na.rm=TRUE),
    accepted_defensive_dpi_yards = sum(dpi_yards, na.rm=TRUE),
    dpi_credit = DPI_WEIGHT * accepted_defensive_dpi_yards,
    adjusted_pass_yards = official_net_pass_yards + dpi_credit,
    adjusted_rush_yards = official_rush_yards,
    official_net_offensive_yards = official_net_pass_yards + official_rush_yards,
    adjusted_offensive_yards = adjusted_pass_yards + adjusted_rush_yards,
    .groups="drop"
  ) |>
  mutate(
    adjusted_pass_share = ifelse(adjusted_offensive_yards > 0,
                                 adjusted_pass_yards / adjusted_offensive_yards, NA_real_)
  ) |>
  arrange(season, season_type, week, game_id, posteam)

# Hard QA: the Yardage input contains yardage only. No points/fair-score/situation fields.
forbidden <- grep("actual_points|fair|situation|sit_", names(team_games), ignore.case=TRUE, value=TRUE)
if (length(forbidden)) stop("Forbidden non-yardage fields found: ", paste(forbidden, collapse=", "))

# Hard QA: adjusted total must equal adjusted pass + rush exactly.
err <- max(abs(team_games$adjusted_offensive_yards -
               (team_games$adjusted_pass_yards + team_games$adjusted_rush_yards)), na.rm=TRUE)
if (!is.finite(err) || err > 1e-9) stop("Total-yard identity failed; max error = ", err)

# Expected scope: 2025 = 285 games / 570 team-games; 2026 W1-3 = 48 games / 96 team-games.
counts <- team_games |>
  group_by(season, season_type) |>
  summarise(games=n_distinct(game_id), team_games=n(), .groups="drop")

c25 <- team_games |> filter(season == 2025)
c26 <- team_games |> filter(season == 2026)
if (n_distinct(c25$game_id) != 285 || nrow(c25) != 570)
  stop("2025 scope mismatch: expected 285 games / 570 team-games")
if (n_distinct(c26$game_id) != 48 || nrow(c26) != 96)
  stop("2026 W1-3 scope mismatch: expected 48 games / 96 team-games")

write.csv(team_games, "pure_yardage_inputs_2025_2026w1-3.csv", row.names=FALSE)
write.csv(dpi_audit, "pure_yardage_dpi_audit_2025_2026w1-3.csv", row.names=FALSE)
write.csv(counts, "pure_yardage_scope_counts.csv", row.names=FALSE)

# GB-ATL Week 3 permanent audit case.
gbatl <- team_games |>
  filter(season == 2026, week == 3, posteam %in% c("GB", "ATL")) |>
  select(season, week, game_id, posteam,
         official_net_pass_yards, official_rush_yards,
         accepted_defensive_dpi_yards, dpi_credit,
         adjusted_pass_yards, adjusted_rush_yards, adjusted_offensive_yards,
         adjusted_pass_share)
write.csv(gbatl, "GB_ATL_week3_pure_yardage_audit.csv", row.names=FALSE)

summary_lines <- c(
  "NFL Numbers — canonical PURE Yardage inputs",
  "============================================",
  "Scope: 2025 REG+POST and 2026 REG Weeks 1-3",
  "Yardage only: no Actual points, Situation or blended fair score",
  "Pass = official net passing yards + 50% accepted defensive DPI yards drawn",
  "Rush = official rushing yards",
  "Adjusted total = adjusted pass + rush",
  "40% split regression is deliberately NOT applied in this extraction file:",
  "it must be applied sequentially using each team's genuine PRE-GAME expected pass/rush split.",
  "This prevents look-ahead and prevents using a fabricated historical expectation.",
  "",
  paste("2025 games:", n_distinct(c25$game_id), "team-games:", nrow(c25)),
  paste("2026 W1-3 games:", n_distinct(c26$game_id), "team-games:", nrow(c26)),
  paste("Accepted DPI events:", nrow(dpi_audit)),
  paste("Total 50% DPI credit:", sum(team_games$dpi_credit), "yards")
)
writeLines(summary_lines, "pure_yardage_summary.txt")
cat(paste(summary_lines, collapse="\n"), "\n")
cat("\nGB-ATL W3 audit:\n")
print(gbatl)
