# Wrapper generated for NFL Numbers.
# Uses the repository's canonical_situation_v3.R verbatim as source, changing ONLY
# 2025 scope from REG to REG+POST and writing a dedicated output directory.
src <- readLines("canonical_situation_v3.R", warn=FALSE)

# Safety: require locked early-down bonus markers before proceeding.
required <- c(
  "Locked specification through 2026-09-28.",
  "early_down_fd_bonus <- function",
  "if (d == 1) return(min(1.00, 0.50 + 0.30*log(ratio)))",
  "min(0.70, 0.30 + 0.20*log(ratio))",
  "dpi_yardage_credit=.5*dpi_yards_drawn"
)
txt <- paste(src, collapse="\n")
for (x in required) if (!grepl(x, txt, fixed=TRUE))
  stop("Canonical Situation file is not the expected locked version; missing: ", x)

# Dedicated output directory.
src <- sub('OUT <- "situation_v3_full"',
           'OUT <- "situation_v3_2025_reg_post"', src, fixed=TRUE)

# In prepare(), replace the single REG filter with season-aware scope.
old <- 'p <- nflreadr::load_pbp(season) |> filter(season_type=="REG")'
new <- paste0(
  'p <- nflreadr::load_pbp(season) |> ',
  'filter(if (season==2025) season_type %in% c("REG","POST") else season_type=="REG")'
)
if (!any(grepl(old, src, fixed=TRUE))) stop("Expected REG filter not found")
src <- sub(old, new, src, fixed=TRUE)

# Existing 2025 week<=18 line would throw out playoff week numbers. Remove it.
src <- src[!grepl('if (season==2025) p <- p |> filter(week<=18)', src, fixed=TRUE)]

# The canonical script's team-game output does not retain season_type.
# Add it to grouping/join keys so playoff week labels can never collide with REG.
# nflverse game_id is unique anyway; this is extra audit protection.
# Instead of invasive patching, game_id remains the canonical unique key and we
# append season_type after execution by matching raw PBP game IDs.

writeLines(src, "canonical_situation_v3_reg_post_tmp.R")
source("canonical_situation_v3_reg_post_tmp.R")

# Enrich the produced 2025 table with season_type and validate 285 games/570 team-games.
suppressPackageStartupMessages({library(nflreadr); library(dplyr)})
meta <- nflreadr::load_pbp(2025) |>
  filter(season_type %in% c("REG","POST")) |>
  distinct(game_id, season_type)

f <- file.path("situation_v3_2025_reg_post","situation_v3_2025_team_games.csv")
x <- read.csv(f, stringsAsFactors=FALSE)
x <- x |> left_join(meta, by="game_id") |>
  relocate(season_type, .after=game_id)

if (length(unique(x$game_id)) != 285) stop("Expected 285 games, got ", length(unique(x$game_id)))
if (nrow(x) != 570) stop("Expected 570 team-games, got ", nrow(x))
if (sum(x$season_type=="POST") != 26) stop("Expected 26 playoff team-games")

write.csv(x, f, row.names=FALSE)

writeLines(c(
  "2025 canonical Situation v3 REG + POST export",
  "Formula source: canonical_situation_v3.R from this repository",
  "Only historical scope was changed: 2025 REG -> 2025 REG + POST.",
  "Locked early-down first-down bonus verified before execution.",
  "Yardage DPI credit 0.5x verified before execution.",
  paste("Games:", length(unique(x$game_id))),
  paste("Team-games:", nrow(x)),
  paste("Playoff team-games:", sum(x$season_type=="POST"))
), file.path("situation_v3_2025_reg_post","REG_POST_README.txt"))
