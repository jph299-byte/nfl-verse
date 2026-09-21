options(stringsAsFactors = FALSE)

# ============================================================
# NFL NUMBERS — WEEKLY SITUATIONAL FAIR-SCORE ENGINE
# ============================================================
#
# Usage:
#
#   Rscript convert_week.R
#   Rscript convert_week.R auto
#   Rscript convert_week.R 1
#   Rscript convert_week.R 2
#
# MODEL
#
# 1st down target = 40% of yards-to-go
# 2nd down target = 60% of yards-to-go
# 3rd/4th target = conversion distance
#
# Failed 3rd/4th:
#   max(-1.5, achievement - 1)
#
# Otherwise achievement <= 1:
#   max(-1.5, achievement)
#
# Achievement > 1:
#   min(1.75, 1 + 0.35 * ln(achievement))
#
# Any fumble = -1
# Interception = -1 and offensive gain reset to zero
# Kneel = 0
#
# PENALTIES
#
# Football gain = 100%
# Accepted live-possession penalty yards = 50%
# Declined / offsetting = 0 penalty effect
# Dead-ball / post-play / between-downs = 0 penalty effect
# Kickoff / PAT / non-offensive enforcement = 0 Situation effect
#
# Accepted live penalty no-plays are retained.
#
# Defensive penalties converting 3rd/4th down preserve the
# conversion benefit despite the 50% yardage treatment.
#
# CALIBRATION
#
#   Situational Fair Points =
#       8.59 + 0.5369 * Raw Situation
#
# Final fair score remains downstream:
#
#   30% Actual
# + 30% Official Net Yardage / 14.5
# + 40% Situational Fair Points
#
# ============================================================

SEASON <- 2026

SITUATION_INTERCEPT <- 8.59
SITUATION_SLOPE <- 0.5369
PENALTY_YARD_WEIGHT <- 0.50

BASE_URL <- paste0(
  "https://github.com/nflverse/nflverse-pbp/releases/download/raw_pbp_",
  SEASON
)

# ------------------------------------------------------------
# ARGUMENT
# ------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

requested_week <- if (length(args)) args[1] else "auto"

if (is.na(requested_week) || requested_week == "")
  requested_week <- "auto"

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------

as_num <- function(x) suppressWarnings(as.numeric(x))

as_flag <- function(x) {

  if (is.logical(x))
    return(!is.na(x) & x)

  tolower(as.character(x)) %in% c(
    "true",
    "1",
    "yes"
  )
}

pick_col <- function(df, candidates, required = TRUE) {

  hit <- candidates[candidates %in% names(df)]

  if (length(hit))
    return(hit[1])

  if (required) {
    stop(
      "Missing expected column. Tried: ",
      paste(candidates, collapse = ", ")
    )
  }

  NA_character_
}

display_team <- function(z) {

  z[z == "LA"] <- "LAR"
  z[z == "JAX"] <- "JAC"

  z
}

# ------------------------------------------------------------
# FINAL RULING FROM REVIEWED PLAY
# ------------------------------------------------------------

final_ruling <- function(txt) {

  if (is.na(txt))
    return("")

  loc <- gregexpr(
    "(?i)REVERSED\\.",
    txt,
    perl = TRUE
  )[[1]]

  if (loc[1] != -1) {

    last <- tail(loc, 1)
    ml <- attr(loc, "match.length")

    return(
      substr(
        txt,
        last + ml[length(ml)],
        nchar(txt)
      )
    )
  }

  txt
}

# ------------------------------------------------------------
# CANONICAL PLAY VALUE
# ------------------------------------------------------------

play_value <- function(
  down,
  togo,
  gain,
  turnover = FALSE,
  kneel = FALSE,
  force_conversion = FALSE
) {

  if (isTRUE(kneel))
    return(0)

  if (
    is.na(down) ||
    is.na(togo) ||
    is.na(gain) ||
    togo <= 0 ||
    !(down %in% 1:4)
  )
    return(NA_real_)

  target <-
    if (down == 1) {

      0.4 * togo

    } else if (down == 2) {

      0.6 * togo

    } else {

      togo
    }

  r <- gain / target

  if (
    isTRUE(force_conversion) &&
    down %in% c(3, 4)
  ) {

    # The defensive penalty converted the down.
    # Preserve conversion credit even though only 50% of the
    # penalty yardage enters effective gain.

    r_conv <- max(1, r)

    v <- min(
      1.75,
      1 + 0.35 * log(r_conv)
    )

  } else if (
    down %in% c(3, 4) &&
    gain < togo
  ) {

    v <- max(
      -1.5,
      r - 1
    )

  } else if (r <= 1) {

    v <- max(
      -1.5,
      r
    )

  } else {

    v <- min(
      1.75,
      1 + 0.35 * log(r)
    )
  }

  if (isTRUE(turnover))
    v <- v - 1

  v
}

# ------------------------------------------------------------
# PENALTY PARSER
# ------------------------------------------------------------

penalty_details <- function(
  txt,
  offense_team,
  raw_gain
) {

  if (is.na(txt))
    txt <- ""

  ruling <- final_ruling(txt)

  has_penalty <- grepl(
    "PENALTY",
    ruling,
    ignore.case = TRUE
  )

  if (!has_penalty) {

    return(list(
      has_penalty = FALSE,
      accepted = FALSE,
      live = FALSE,
      side = "none",
      yards = 0,
      football_gain = raw_gain,
      penalty_effect = 0,
      force_conversion = FALSE
    ))
  }

  declined <- grepl(
    "declined|offsetting|offset",
    ruling,
    ignore.case = TRUE
  )

  dead_ball <- grepl(
    paste(
      "dead ball",
      "between downs",
      "after the play",
      "after play",
      "post-play",
      "post play",
      "enforced on kickoff",
      "enforced at kickoff",
      "ensuing kickoff",
      "extra point",
      "PAT",
      sep = "|"
    ),
    ruling,
    ignore.case = TRUE
  )

  # ----------------------------------------------------------
  # TEAM CHARGED WITH PENALTY
  # ----------------------------------------------------------

  penalty_team <- NA_character_

  patterns <- c(
    "(?i)PENALTY\\s+on\\s+([A-Z]{2,3})[- ,:]",
    "(?i)PENALTY,?\\s+([A-Z]{2,3})[- ,:]"
  )

  for (pat in patterns) {

    m <- regexec(
      pat,
      ruling,
      perl = TRUE
    )

    z <- regmatches(
      ruling,
      m
    )[[1]]

    if (length(z) >= 2) {

      penalty_team <- toupper(z[2])
      break
    }
  }

  # ----------------------------------------------------------
  # PENALTY YARDS
  # ----------------------------------------------------------

  penalty_section <- sub(
    "(?is)^.*?PENALTY",
    "PENALTY",
    ruling,
    perl = TRUE
  )

  yard_hits <- regmatches(
    penalty_section,
    gregexpr(
      "(?i)([0-9]+)\\s+yards?",
      penalty_section,
      perl = TRUE
    )
  )[[1]]

  penalty_yards <- 0

  if (
    length(yard_hits) &&
    yard_hits[1] != "-1"
  ) {

    penalty_yards <- suppressWarnings(
      as.numeric(
        sub(
          ".*?([0-9]+).*",
          "\\1",
          yard_hits[1]
        )
      )
    )

    if (!is.finite(penalty_yards))
      penalty_yards <- 0
  }

  # ----------------------------------------------------------
  # UNDERLYING FOOTBALL GAIN
  # ----------------------------------------------------------
  #
  # ESPN's final yards field can include enforcement.
  # Recover the actual football gain from the part of the
  # description preceding PENALTY where possible.
  # ----------------------------------------------------------

  football_gain <- raw_gain

  pre_penalty <- sub(
    "(?is)PENALTY.*$",
    "",
    ruling,
    perl = TRUE
  )

  gain_hits <- regmatches(
    pre_penalty,
    gregexpr(
      "(?i)for (no gain|-?[0-9]+ yards?)",
      pre_penalty,
      perl = TRUE
    )
  )[[1]]

  if (
    length(gain_hits) &&
    gain_hits[1] != "-1"
  ) {

    h <- tail(
      gain_hits,
      1
    )

    if (
      grepl(
        "no gain",
        h,
        ignore.case = TRUE
      )
    ) {

      football_gain <- 0

    } else {

      val <- suppressWarnings(
        as.numeric(
          sub(
            ".*?(-?[0-9]+).*",
            "\\1",
            h
          )
        )
      )

      if (is.finite(val))
        football_gain <- val
    }
  }

  accepted <-
    has_penalty &&
    !declined

  live <-
    accepted &&
    !dead_ball &&
    penalty_yards > 0

  side <- "unknown"

  if (!is.na(penalty_team)) {

    if (penalty_team == offense_team) {

      side <- "offense"

    } else {

      side <- "defense"
    }
  }

  penalty_effect <- 0

  if (
    live &&
    side == "offense"
  ) {

    penalty_effect <-
      -PENALTY_YARD_WEIGHT *
      penalty_yards
  }

  if (
    live &&
    side == "defense"
  ) {

    penalty_effect <-
      PENALTY_YARD_WEIGHT *
      penalty_yards
  }

  automatic_first <- grepl(
    "automatic first down|first down",
    penalty_section,
    ignore.case = TRUE
  )

  force_conversion <-
    live &&
    side == "defense" &&
    automatic_first

  list(
    has_penalty = has_penalty,
    accepted = accepted,
    live = live,
    side = side,
    yards = penalty_yards,
    football_gain = football_gain,
    penalty_effect = penalty_effect,
    force_conversion = force_conversion
  )
}

# ============================================================
# RELEASE MANIFEST
# ============================================================

get_release_text <- function() {

  release_url <- paste0(
    "https://api.github.com/repos/nflverse/nflverse-pbp/releases/tags/raw_pbp_",
    SEASON
  )

  tmp <- tempfile(
    fileext = ".json"
  )

  download.file(
    release_url,
    tmp,
    quiet = TRUE
  )

  txt <- paste(
    readLines(
      tmp,
      warn = FALSE
    ),
    collapse = "\n"
  )

  unlink(tmp)

  txt
}

# ============================================================
# DISCOVER LATEST AVAILABLE WEEK
# ============================================================

find_latest_week <- function() {

  cat(
    "Finding latest available nflverse week...\n"
  )

  txt <- get_release_text()

  pattern <- paste0(
    SEASON,
    "_([0-9]{2})_[A-Z0-9]+_[A-Z0-9]+\\.rds"
  )

  hits <- gregexpr(
    pattern,
    txt,
    perl = TRUE
  )

  files <- regmatches(
    txt,
    hits
  )[[1]]

  if (
    !length(files) ||
    identical(files, character(0))
  ) {

    stop(
      "Could not discover any ",
      SEASON,
      " nflverse game files."
    )
  }

  weeks <- as.integer(
    sub(
      paste0(
        "^",
        SEASON,
        "_([0-9]{2})_.*$"
      ),
      "\\1",
      files
    )
  )

  max(
    weeks,
    na.rm = TRUE
  )
}

if (
  tolower(requested_week) == "auto"
) {

  WEEK <- find_latest_week()

} else {

  WEEK <- suppressWarnings(
    as.integer(requested_week)
  )

  if (
    is.na(WEEK) ||
    WEEK < 1 ||
    WEEK > 22
  ) {

    stop(
      "Invalid week: ",
      requested_week
    )
  }
}

cat("\n========================================\n")

cat(
  "NFL Numbers — Season ",
  SEASON,
  "\n",
  sep = ""
)

cat(
  "Processing Week ",
  WEEK,
  "\n",
  sep = ""
)

cat(
  "Situation calibration: ",
  SITUATION_INTERCEPT,
  " + ",
  SITUATION_SLOPE,
  " × raw\n",
  sep = ""
)

cat(
  "Live penalty weight: ",
  PENALTY_YARD_WEIGHT,
  "\n",
  sep = ""
)

cat("========================================\n\n")

writeLines(
  as.character(WEEK),
  "output_week.txt"
)

# ============================================================
# DISCOVER ALL GAME FILES FOR WEEK
# ============================================================

discover_games <- function(week) {

  txt <- get_release_text()

  prefix <- sprintf(
    "%d_%02d_",
    SEASON,
    week
  )

  pattern <- paste0(
    prefix,
    "[A-Z0-9]+_[A-Z0-9]+\\.rds"
  )

  hits <- gregexpr(
    pattern,
    txt,
    perl = TRUE
  )

  files <- unique(
    regmatches(
      txt,
      hits
    )[[1]]
  )

  if (
    !length(files) ||
    identical(files, character(0))
  ) {

    stop(
      "No nflverse game files available for Week ",
      week
    )
  }

  games <- sub(
    paste0("^", prefix),
    "",
    files
  )

  games <- sub(
    "\\.rds$",
    "",
    games
  )

  sort(unique(games))
}

GAMES <- discover_games(WEEK)

cat(
  "Games currently available: ",
  length(GAMES),
  "\n",
  sep = ""
)

cat(
  paste(
    GAMES,
    collapse = ", "
  ),
  "\n\n"
)

# ============================================================
# OUTPUT DIRECTORIES
# ============================================================

week_dir <- file.path(
  "output",
  sprintf(
    "week%02d",
    WEEK
  )
)

rds_dir <- file.path(
  week_dir,
  "rds"
)

play_dir <- file.path(
  week_dir,
  "plays"
)

drive_dir <- file.path(
  week_dir,
  "drives"
)

dir.create(
  rds_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  play_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  drive_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "csv",
  showWarnings = FALSE
)

old_csv <- list.files(
  "csv",
  full.names = TRUE
)

if (length(old_csv))
  unlink(old_csv)

# ============================================================
# DOWNLOAD GAME
# ============================================================

download_game <- function(g) {

  fn <- sprintf(
    "%d_%02d_%s.rds",
    SEASON,
    WEEK,
    g
  )

  dest <- file.path(
    rds_dir,
    fn
  )

  if (!file.exists(dest)) {

    url <- paste0(
      BASE_URL,
      "/",
      fn
    )

    cat(
      "Downloading ",
      url,
      "\n",
      sep = ""
    )

    download.file(
      url,
      dest,
      mode = "wb",
      quiet = FALSE
    )
  }

  if (
    !file.exists(dest) ||
    file.info(dest)$size == 0
  ) {

    stop(
      "Bad download: ",
      g
    )
  }

  dest
}

# ============================================================
# EXTRACT + SCORE GAME
# ============================================================

extract_game <- function(path, g) {

  x <- readRDS(path)

  gd <- x$data$viewer$gameDetail

  if (
    is.null(gd) ||
    is.null(gd$plays) ||
    !is.data.frame(gd$plays)
  ) {

    stop(
      "Could not find gameDetail$plays for ",
      g
    )
  }

  p <- gd$plays

  # ----------------------------------------------------------
  # COLUMN MAPPING
  # ----------------------------------------------------------

  team_col <- pick_col(
    p,
    c(
      "possessionTeam.abbreviation",
      "possessionTeamAbbreviation",
      "possessionTeam",
      "posteam"
    )
  )

  down_col <- pick_col(
    p,
    c(
      "down",
      "downNumber"
    )
  )

  togo_col <- pick_col(
    p,
    c(
      "yardsToGo",
      "ydstogo"
    )
  )

  yards_col <- pick_col(
    p,
    c(
      "yards",
      "yardsGained",
      "yards_gained"
    )
  )

  desc_col <- pick_col(
    p,
    c(
      "playDescription",
      "shortDescription",
      "desc"
    ),
    FALSE
  )

  deleted_col <- pick_col(
    p,
    c(
      "playDeleted",
      "noPlay",
      "no_play"
    ),
    FALSE
  )

  st_col <- pick_col(
    p,
    c(
      "specialTeamsPlay",
      "specialTeams"
    ),
    FALSE
  )

  drive_col <- pick_col(
    p,
    c(
      "driveId",
      "drive.id",
      "driveNumber",
      "drive"
    ),
    FALSE
  )

  quarter_col <- pick_col(
    p,
    c(
      "quarter",
      "period",
      "quarterNumber"
    ),
    FALSE
  )

  clock_col <- pick_col(
    p,
    c(
      "gameClock",
      "clock",
      "time"
    ),
    FALSE
  )

  play_id_col <- pick_col(
    p,
    c(
      "playId",
      "play.id",
      "id"
    ),
    FALSE
  )

  # ----------------------------------------------------------
  # RAW VALUES
  # ----------------------------------------------------------

  team <- as.character(
    p[[team_col]]
  )

  down <- as_num(
    p[[down_col]]
  )

  togo <- as_num(
    p[[togo_col]]
  )

  gain <- as_num(
    p[[yards_col]]
  )

  desc <-
    if (!is.na(desc_col))
      as.character(
        p[[desc_col]]
      )
    else
      rep(
        "",
        nrow(p)
      )

  deleted <-
    if (!is.na(deleted_col))
      as_flag(
        p[[deleted_col]]
      )
    else
      rep(
        FALSE,
        nrow(p)
      )

  special <-
    if (!is.na(st_col))
      as_flag(
        p[[st_col]]
      )
    else
      rep(
        FALSE,
        nrow(p)
      )

  # ----------------------------------------------------------
  # RETAIN VALID OFFENSIVE PLAYS
  # ----------------------------------------------------------

  no_play_text <- grepl(
    "No Play|NO PLAY",
    desc
  )

  penalty_text <- grepl(
    "PENALTY",
    desc,
    ignore.case = TRUE
  )

  declined_text <- grepl(
    "declined|offsetting|offset",
    desc,
    ignore.case = TRUE
  )

  dead_penalty_text <- grepl(
    paste(
      "dead ball",
      "between downs",
      "after the play",
      "after play",
      "post-play",
      "post play",
      "enforced on kickoff",
      "enforced at kickoff",
      "ensuing kickoff",
      "extra point",
      "PAT",
      sep = "|"
    ),
    desc,
    ignore.case = TRUE
  )

  retain_penalty_no_play <-
    penalty_text &
    !declined_text &
    !dead_penalty_text

  base_valid <-
    !special &
    !is.na(team) &
    team != "" &
    !is.na(down) &
    down %in% 1:4 &
    !is.na(togo) &
    togo > 0 &
    !is.na(gain)

  keep <-
    base_valid &
    (
      (!deleted & !no_play_text) |
      retain_penalty_no_play
    )

  # ----------------------------------------------------------
  # AUDIT TABLE
  # ----------------------------------------------------------

  q <- data.frame(
    source_row = which(keep),
    team = team[keep],
    down = down[keep],
    yards_to_go = togo[keep],
    raw_gain = gain[keep],
    description = desc[keep],
    stringsAsFactors = FALSE
  )

  q$play_id <-
    if (!is.na(play_id_col))
      as.character(
        p[[play_id_col]][keep]
      )
    else
      as.character(
        q$source_row
      )

  q$drive <-
    if (!is.na(drive_col))
      as.character(
        p[[drive_col]][keep]
      )
    else
      NA_character_

  q$quarter <-
    if (!is.na(quarter_col))
      as.character(
        p[[quarter_col]][keep]
      )
    else
      NA_character_

  q$clock <-
    if (!is.na(clock_col))
      as.character(
        p[[clock_col]][keep]
      )
    else
      NA_character_

  # ----------------------------------------------------------
  # FINAL RULINGS
  # ----------------------------------------------------------

  ruling <- vapply(
    q$description,
    final_ruling,
    character(1)
  )

  # ----------------------------------------------------------
  # PENALTY TREATMENT
  # ----------------------------------------------------------

  pd <- lapply(
    seq_len(nrow(q)),
    function(i) {
      penalty_details(
        q$description[i],
        q$team[i],
        q$raw_gain[i]
      )
    }
  )

  q$penalty <- vapply(
    pd,
    function(x) x$has_penalty,
    logical(1)
  )

  q$penalty_accepted <- vapply(
    pd,
    function(x) x$accepted,
    logical(1)
  )

  q$penalty_live <- vapply(
    pd,
    function(x) x$live,
    logical(1)
  )

  q$penalty_side <- vapply(
    pd,
    function(x) x$side,
    character(1)
  )

  q$penalty_yards <- vapply(
    pd,
    function(x) x$yards,
    numeric(1)
  )

  q$football_gain <- vapply(
    pd,
    function(x) x$football_gain,
    numeric(1)
  )

  q$penalty_effect <- vapply(
    pd,
    function(x) x$penalty_effect,
    numeric(1)
  )

  q$force_conversion <- vapply(
    pd,
    function(x) x$force_conversion,
    logical(1)
  )

  q$gain <-
    q$football_gain +
    q$penalty_effect

  # ----------------------------------------------------------
  # TURNOVERS
  # ----------------------------------------------------------

  q$interception <- grepl(
    "INTERCEPTED|intercepted by",
    ruling,
    ignore.case = TRUE
  )

  q$fumble <- grepl(
    "FUMBLES|Fumble",
    ruling,
    ignore.case = TRUE
  )

  # Interception return yards are not offensive yards.

  q$gain[
    q$interception
  ] <- 0

  q$gain_source <- "raw/penalty-adjusted"

  q$gain_source[
    q$interception
  ] <- "interception=0"

  # ----------------------------------------------------------
  # FUMBLE GAIN RECONSTRUCTION
  # ----------------------------------------------------------

  event_idx <- which(
    q$fumble
  )

  for (i in event_idx) {

    txt <- ruling[i]

    pre <- sub(
      "(?i)FUMBLES?.*$",
      "",
      txt,
      perl = TRUE
    )

    mm <- gregexpr(
      "for (no gain|-?[0-9]+ yards?)",
      pre,
      perl = TRUE,
      ignore.case = TRUE
    )

    hits <- regmatches(
      pre,
      mm
    )[[1]]

    if (
      length(hits) &&
      hits[1] != "-1"
    ) {

      h <- tail(
        hits,
        1
      )

      if (
        grepl(
          "no gain",
          h,
          ignore.case = TRUE
        )
      ) {

        val <- 0

      } else {

        val <- as.numeric(
          sub(
            ".*?(-?[0-9]+).*",
            "\\1",
            h
          )
        )
      }

      if (is.finite(val)) {

        # Retain any legitimate live penalty effect.
        q$football_gain[i] <- val

        q$gain[i] <-
          val +
          q$penalty_effect[i]

        q$gain_source[i] <-
          "pre-fumble final ruling"
      }

    } else if (
      grepl(
        "FUMBLES \\(Aborted\\)",
        txt,
        ignore.case = TRUE
      )
    ) {

      q$gain_source[i] <-
        "raw-aborted"

    } else {

      q$gain_source[i] <-
        "MANUAL CHECK"
    }
  }

  # ----------------------------------------------------------
  # SCORE EACH PLAY
  # ----------------------------------------------------------

  q$turnover_event <-
    q$interception |
    q$fumble

  q$kneel <- grepl(
    "kneel|kneels",
    ruling,
    ignore.case = TRUE
  )

  q$target <- ifelse(
    q$down == 1,
    0.4 * q$yards_to_go,
    ifelse(
      q$down == 2,
      0.6 * q$yards_to_go,
      q$yards_to_go
    )
  )

  q$achievement <-
    q$gain /
    q$target

  q$play_value <- mapply(
    play_value,
    q$down,
    q$yards_to_go,
    q$gain,
    q$turnover_event,
    q$kneel,
    q$force_conversion
  )

  # ----------------------------------------------------------
  # GAME INFO
  # ----------------------------------------------------------

  away <- as.character(
    gd$visitorTeam$abbreviation
  )

  home <- as.character(
    gd$homeTeam$abbreviation
  )

  actual_away <- as_num(
    gd$visitorPointsTotal
  )

  actual_home <- as_num(
    gd$homePointsTotal
  )

  if (
    !length(actual_away) ||
    !length(actual_home) ||
    !is.finite(actual_away[1]) ||
    !is.finite(actual_home[1])
  ) {

    stop(
      "Game does not appear complete: ",
      g
    )
  }

  actual_away <- actual_away[1]
  actual_home <- actual_home[1]

  raw_away <- sum(
    q$play_value[
      q$team == away
    ],
    na.rm = TRUE
  )

  raw_home <- sum(
    q$play_value[
      q$team == home
    ],
    na.rm = TRUE
  )

  away_plays <- sum(
    q$team == away
  )

  home_plays <- sum(
    q$team == home
  )

  # ----------------------------------------------------------
  # NEW LOCKED REGRESSION CALIBRATION
  # ----------------------------------------------------------

  situ_away <-
    SITUATION_INTERCEPT +
    SITUATION_SLOPE *
    raw_away

  situ_home <-
    SITUATION_INTERCEPT +
    SITUATION_SLOPE *
    raw_home

  # ----------------------------------------------------------
  # AUDIT COUNTS
  # ----------------------------------------------------------

  manual_n <- sum(
    q$gain_source ==
      "MANUAL CHECK"
  )

  unknown_penalty_n <- sum(
    q$penalty_live &
    q$penalty_side == "unknown"
  )

    penalty_no_play_n <- sum(
    q$penalty_accepted &
    grepl(
      "No Play|NO PLAY",
      q$description
    )
  )

  conversion_override_n <- sum(
    q$force_conversion
  )

  # ----------------------------------------------------------
  # WRITE PLAY AUDIT
  # ----------------------------------------------------------

  play_file <- file.path(
    play_dir,
    paste0(
      g,
      "_exact_plays.csv"
    )
  )

  write.csv(
    q,
    play_file,
    row.names = FALSE
  )

  write.csv(
    q,
    file.path(
      "csv",
      paste0(
        g,
        "_exact_plays.csv"
      )
    ),
    row.names = FALSE
  )

  # ----------------------------------------------------------
  # DRIVE AUDIT
  # ----------------------------------------------------------

  if (!all(is.na(q$drive))) {

    drive_key <- paste(
      q$team,
      q$drive,
      sep = "_"
    )

    drive_split <- split(
      q,
      drive_key
    )

    drives <- do.call(
      rbind,
      lapply(
        drive_split,
        function(d) {

          drive_raw <- sum(
            d$play_value,
            na.rm = TRUE
          )

          data.frame(
            team = d$team[1],
            drive = d$drive[1],
            quarter = d$quarter[1],
            start_clock = d$clock[1],
            plays = nrow(d),

            football_yards = sum(
              d$football_gain,
              na.rm = TRUE
            ),

            penalty_effect_yards = sum(
              d$penalty_effect,
              na.rm = TRUE
            ),

            effective_situation_yards = sum(
              d$gain,
              na.rm = TRUE
            ),

            raw_situational = drive_raw,

            calibrated_situational =
              SITUATION_INTERCEPT +
              SITUATION_SLOPE * drive_raw,

            stringsAsFactors = FALSE
          )
        }
      )
    )

    rownames(drives) <- NULL

    write.csv(
      drives,
      file.path(
        drive_dir,
        paste0(
          g,
          "_drives.csv"
        )
      ),
      row.names = FALSE
    )
  }

  # ----------------------------------------------------------
  # WARNINGS
  # ----------------------------------------------------------

  if (manual_n) {

    cat(
      "MANUAL CHECK plays for ",
      g,
      ": ",
      manual_n,
      "\n",
      sep = ""
    )
  }

  if (unknown_penalty_n) {

    cat(
      "UNKNOWN LIVE PENALTY SIDE for ",
      g,
      ": ",
      unknown_penalty_n,
      "\n",
      sep = ""
    )
  }

  cat(
    sprintf(
      paste0(
        "%-8s plays=%3d ",
        "raw situation %.3f / %.3f ",
        "situ %.3f / %.3f ",
        "penalty no-plays=%d ",
        "conversion overrides=%d ",
        "unknown penalties=%d\n"
      ),
      g,
      nrow(q),
      raw_away,
      raw_home,
      situ_away,
      situ_home,
      penalty_no_play_n,
      conversion_override_n,
      unknown_penalty_n
    )
  )

  # ----------------------------------------------------------
  # GAME RESULT
  # ----------------------------------------------------------

  data.frame(
    week = WEEK,
    game = g,

    away = display_team(away),
    home = display_team(home),

    away_plays = away_plays,
    home_plays = home_plays,

    raw_situ_away = raw_away,
    raw_situ_home = raw_home,

    situ_away = situ_away,
    situ_home = situ_home,

    actual_away = actual_away,
    actual_home = actual_home,

    situation_intercept = SITUATION_INTERCEPT,
    situation_slope = SITUATION_SLOPE,
    penalty_yard_weight = PENALTY_YARD_WEIGHT,

    penalty_no_plays = penalty_no_play_n,
    conversion_overrides = conversion_override_n,
    unknown_live_penalties = unknown_penalty_n,

    quality = ifelse(
      manual_n == 0 &&
      unknown_penalty_n == 0,
      "A",
      "CHECK"
    ),

    manual_checks = manual_n,

    stringsAsFactors = FALSE
  )
}

# ============================================================
# RUN WEEK
# ============================================================

rows <- list()
failed <- character()

for (g in GAMES) {

  cat(
    "\n=== ",
    g,
    " ===\n",
    sep = ""
  )

  tryCatch({

    rows[[g]] <-
      extract_game(
        download_game(g),
        g
      )

  }, error = function(e) {

    cat(
      "SKIPPED ",
      g,
      ": ",
      conditionMessage(e),
      "\n",
      sep = ""
    )

    failed <<- c(
      failed,
      g
    )
  })
}

if (!length(rows)) {
  stop(
    "No completed games were successfully processed."
  )
}

res <- do.call(
  rbind,
  rows
)

rownames(res) <- NULL

# ============================================================
# VALIDATION
# ============================================================

if (
  any(
    !is.finite(
      res$raw_situ_away
    )
  )
)
  stop(
    "Non-finite away raw Situation."
  )

if (
  any(
    !is.finite(
      res$raw_situ_home
    )
  )
)
  stop(
    "Non-finite home raw Situation."
  )

expected_away <-
  SITUATION_INTERCEPT +
  SITUATION_SLOPE *
  res$raw_situ_away

expected_home <-
  SITUATION_INTERCEPT +
  SITUATION_SLOPE *
  res$raw_situ_home

if (
  any(
    abs(
      res$situ_away -
      expected_away
    ) > 1e-8
  )
)
  stop(
    "Away Situation calibration failed."
  )

if (
  any(
    abs(
      res$situ_home -
      expected_home
    ) > 1e-8
  )
)
  stop(
    "Home Situation calibration failed."
  )

if (
  any(
    res$unknown_live_penalties > 0
  )
) {

  cat(
    "\nWARNING: at least one game has an ",
    "unclassified live penalty.\n"
  )
}

# ============================================================
# OUTPUT
# ============================================================

results_file <- sprintf(
  "week%d_situation_results.csv",
  WEEK
)

write.csv(
  res,
  results_file,
  row.names = FALSE
)

write.csv(
  res,
  file.path(
    week_dir,
    "situation_results.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# JAVASCRIPT OUTPUT
# ------------------------------------------------------------

js <- apply(
  res,
  1,
  function(z) {

    sprintf(
      paste0(
        '  {away:"%s",home:"%s",',
        'actual:[%.3f,%.3f],',
        'rawSitu:[%.3f,%.3f],',
        'situ:[%.3f,%.3f],',
        'quality:"%s"}'
      ),

      z["away"],
      z["home"],

      as.numeric(
        z["actual_away"]
      ),

      as.numeric(
        z["actual_home"]
      ),

      as.numeric(
        z["raw_situ_away"]
      ),

      as.numeric(
        z["raw_situ_home"]
      ),

      as.numeric(
        z["situ_away"]
      ),

      as.numeric(
        z["situ_home"]
      ),

      z["quality"]
    )
  }
)

js_name <- paste0(
  "WEEK",
  WEEK,
  "_SITUATION_RESULTS"
)

js_lines <- c(
  paste0(
    "const ",
    js_name,
    " = ["
  ),

  paste(
    js,
    collapse = ",\n"
  ),

  "];"
)

js_file <- sprintf(
  "week%d_situation_results.js",
  WEEK
)

writeLines(
  js_lines,
  js_file
)

writeLines(
  js_lines,
  file.path(
    week_dir,
    "situation_results.js"
  )
)

# ============================================================
# SUMMARY
# ============================================================

cat("\n========================================\n")

cat(
  "SUCCESS — NFL Numbers Week ",
  WEEK,
  "\n",
  sep = ""
)

cat("========================================\n")

cat(
  "Completed games processed: ",
  nrow(res),
  "\n",
  sep = ""
)

cat(
  "Situation calibration: ",
  SITUATION_INTERCEPT,
  " + ",
  SITUATION_SLOPE,
  " × raw\n",
  sep = ""
)

cat(
  "Accepted live penalty yard weight: ",
  PENALTY_YARD_WEIGHT,
  "\n",
  sep = ""
)

cat(
  "Accepted penalty no-plays retained: ",
  sum(
    res$penalty_no_plays
  ),
  "\n",
  sep = ""
)

cat(
  "Defensive conversion overrides: ",
  sum(
    res$conversion_overrides
  ),
  "\n",
  sep = ""
)

cat(
  "Unknown live penalties: ",
  sum(
    res$unknown_live_penalties
  ),
  "\n",
  sep = ""
)

if (length(failed)) {

  cat(
    "Available games skipped: ",
    paste(
      failed,
      collapse = ", "
    ),
    "\n",
    sep = ""
  )
}

cat("\n")

print(
  res[
    ,
    c(
      "game",
      "actual_away",
      "actual_home",
      "raw_situ_away",
      "raw_situ_home",
      "situ_away",
      "situ_home",
      "penalty_no_plays",
      "conversion_overrides",
      "unknown_live_penalties",
      "quality",
      "manual_checks"
    )
  ]
)

cat(
  "\nFinal 30/30/40 fair scores are NOT calculated here.\n"
)

cat(
  "Use verified official net offensive yardage separately ",
  "before calculating the final fair score.\n"
)
