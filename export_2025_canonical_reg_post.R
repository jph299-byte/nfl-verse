# NFL Numbers — canonical 2025 REG + POST wrapper (fixed)
# Uses canonical_situation_v3.R as the formula source and changes only 2025 scope.

src <- readLines("canonical_situation_v3.R", warn=FALSE)
txt <- paste(src, collapse="\n")

required <- c(
  "Locked specification through 2026-09-28.",
  "early_down_fd_bonus <- function",
  "if (d == 1) return(min(1.00, 0.50 + 0.30*log(ratio)))",
  "min(0.70, 0.30 + 0.20*log(ratio))",
  "dpi_yardage_credit=.5*dpi_yards_drawn"
)
for (x in required) if (!grepl(x, txt, fixed=TRUE))
  stop("Canonical Situation file is not the expected locked version; missing: ", x)

src <- sub('OUT <- "situation_v3_full"',
           'OUT <- "situation_v3_2025_reg_post"', src, fixed=TRUE)

# IMPORTANT: use .env$season so dplyr does not confuse the function argument
# `season` with nflverse's `season` column.
old <- 'p <- nflreadr::load_pbp(season) |> filter(season_type=="REG")'
new <- paste0(
  'p <- nflreadr::load_pbp(season) |> ',
  'filter(if (.env$season==2025) season_type %in% c("REG","POST") else season_type=="REG")'
)
if (!any(grepl(old, src, fixed=TRUE))) stop("Expected REG filter not found")
src <- sub(old, new, src, fixed=TRUE)

# Remove regular-season-only week cap for 2025 so postseason is retained.
src <- src[!grepl('if (season==2025) p <- p |> filter(week<=18)', src, fixed=TRUE)]

writeLines(src, "canonical_situation_v3_reg_post_tmp.R")
source("canonical_situation_v3_reg_post_tmp.R")

suppressPackageStartupMessages({library(nflreadr); library(dplyr)})
meta <- nflreadr::load_pbp(2025) |>
  filter(season_type %in% c("REG","POST")) |>
  distinct(game_id, season_type)

f <- file.path("situation_v3_2025_reg_post","situation_v3_2025_team_games.csv")
x <- read.csv(f, stringsAsFactors=FALSE) |>
  left_join(meta, by="game_id") |>
  relocate(season_type, .after=game_id)

if (length(unique(x$game_id)) != 285) stop("Expected 285 games, got ", length(unique(x$game_id)))
if (nrow(x) != 570) stop("Expected 570 team-games, got ", nrow(x))
if (sum(x$season_type=="POST") != 26) stop("Expected 26 playoff team-games")

write.csv(x, f, row.names=FALSE)
writeLines(c(
  "2025 canonical Situation v3 REG + POST export",
  "Formula source: canonical_situation_v3.R",
  "Only historical scope changed: 2025 REG -> REG + POST.",
  "Locked early-down bonus and 0.5x DPI yardage checks passed.",
  paste("Games:", length(unique(x$game_id))),
  paste("Team-games:", nrow(x)),
  paste("Playoff team-games:", sum(x$season_type=="POST"))
), file.path("situation_v3_2025_reg_post","REG_POST_README.txt"))
