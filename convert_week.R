options(stringsAsFactors = FALSE)

# ============================================================
# NFL NUMBERS — AUTOMATIC WEEKLY SITUATIONAL FAIR-SCORE ENGINE
# ============================================================
#
# Usage:
#
#   Rscript convert_week.R
#   Rscript convert_week.R auto
#   Rscript convert_week.R 2
#
# MODEL — DO NOT CHANGE WITHOUT INTENTIONAL RECALIBRATION
#
# Target:
#   1st down = 40% of yards-to-go
#   2nd down = 60% of yards-to-go
#   3rd/4th = conversion distance
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
# Turnover/fumble event penalty = -1.0
# Kneel = 0
#
# Yardage fair points:
#   official net offensive yards / 14.5
#
# Situational fair points:
#   game yardage-fair-point pool distributed according to
#   each team's share of raw situational value
#
# Overall fair score:
#   30% actual + 30% yardage + 40% situational
#
# ============================================================

SEASON <- 2026

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

  tolower(as.character(x)) %in% c("true", "1", "yes")
}

pick_col <- function(df, candidates, required = TRUE) {

  hit <- candidates[candidates %in% names(df)]

  if (length(hit))
    return(hit[1])

  if (required)
    stop(
      "Missing expected column. Tried: ",
      paste(candidates, collapse = ", ")
    )

  NA_character_
}

display_team <- function(z) {

  z[z == "LA"] <- "LAR"
  z[z == "JAX"] <- "JAC"

  z
}

# ------------------------------------------------------------
# CANONICAL PLAY VALUE
# ------------------------------------------------------------

play_value <- function(
  down,
  togo,
  gain,
  turnover = FALSE,
  kneel = FALSE
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

  if (down %in% c(3, 4) && gain < togo) {

    v <- max(-1.5, r - 1)

  } else if (r <= 1) {

    v <- max(-1.5, r)

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

# ============================================================
# DISCOVER AVAILABLE WEEK
# ============================================================

game_file_exists <- function(week, game) {

  fn <- sprintf(
    "%d_%02d_%s.rds",
    SEASON,
    week,
    game
  )

  url <- paste0(BASE_URL, "/", fn)

  tf <- tempfile(fileext = ".rds")

  ok <- tryCatch({

    suppressWarnings(
      download.file(
        url,
        tf,
        mode = "wb",
        quiet = TRUE
      )
    )

    file.exists(tf) &&
      file.info(tf)$size > 1000

  }, error = function(e) FALSE)

  unlink(tf)

  ok
}

# We only use this probe to establish whether a week exists.
# The actual game list is discovered separately below.

find_latest_week <- function() {

  cat("Finding latest available nflverse week...\n")

  # A known Thursday/Sunday game cannot safely be assumed each week,
  # so week discovery is performed through the release manifest below.

  release_url <- paste0(
    "https://api.github.com/repos/nflverse/nflverse-pbp/releases/tags/raw_pbp_",
    SEASON
  )

  tmp <- tempfile(fileext = ".json")

  download.file(
    release_url,
    tmp,
    quiet = TRUE
  )

  txt <- paste(
    readLines(tmp, warn = FALSE),
    collapse = "\n"
  )

  unlink(tmp)

  # Extract filenames such as:
  # 2026_02_DET_BUF.rds

  pattern <- paste0(
    SEASON,
    "_([0-9]{2})_[A-Z0-9]+_[A-Z0-9]+\\.rds"
  )

  hits <- gregexpr(
    pattern,
    txt,
    perl = TRUE
  )

  files <- regmatches(txt, hits)[[1]]

  if (!length(files))
    stop("Could not discover any ", SEASON, " nflverse game files.")

  weeks <- as.integer(
    sub(
      paste0("^", SEASON, "_([0-9]{2})_.*$"),
      "\\1",
      files
    )
  )

  max(weeks, na.rm = TRUE)
}

if (tolower(requested_week) == "auto") {

  WEEK <- find_latest_week()

} else {

  WEEK <- suppressWarnings(
    as.integer(requested_week)
  )

  if (
    is.na(WEEK) ||
    WEEK < 1 ||
    WEEK > 22
  )
    stop("Invalid week: ", requested_week)
}

cat("\n========================================\n")
cat("NFL Numbers — Season ", SEASON, "\n", sep = "")
cat("Processing Week ", WEEK, "\n", sep = "")
cat("========================================\n\n")

writeLines(
  as.character(WEEK),
  "output_week.txt"
)

# ============================================================
# DISCOVER ALL GAME FILES FOR WEEK
# ============================================================

discover_games <- function(week) {

  release_url <- paste0(
    "https://api.github.com/repos/nflverse/nflverse-pbp/releases/tags/raw_pbp_",
    SEASON
  )

  tmp <- tempfile(fileext = ".json")

  download.file(
    release_url,
    tmp,
    quiet = TRUE
  )

  txt <- paste(
    readLines(tmp, warn = FALSE),
    collapse = "\n"
  )

  unlink(tmp)

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
    regmatches(txt, hits)[[1]]
  )

  if (!length(files))
    stop(
      "No nflverse game files available for Week ",
      week
    )

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
  "\n"
)

cat(
  paste(GAMES, collapse = ", "),
  "\n\n"
)

# ============================================================
# OUTPUT DIRECTORIES
# ============================================================

week_dir <- file.path(
  "output",
  sprintf("week%02d", WEEK)
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

# Compatibility folder for the existing workflow/app.
dir.create(
  "csv",
  showWarnings = FALSE
)

# Clear compatibility CSV folder so another week's
# play files cannot leak into this week's artifact.

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
  )
    stop("Bad download: ", g)

  dest
}

# ============================================================
# OFFICIAL NET OFFENSIVE YARDS
# ============================================================

get_team_yards <- function(gd, team, plays) {

  # First try game/team statistical fields if nflverse supplies them.
  #
  # We deliberately only accept fields that clearly describe
  # total/net offensive yards.

  candidates <- c(
    "totalYards",
    "totalNetYards",
    "netYards",
    "netOffensiveYards",
    "yards"
  )

  objects <- list(
    gd$visitorTeam,
    gd$homeTeam
  )

  for (obj in objects) {

    if (is.null(obj))
      next

    abbr <- NULL

    if (!is.null(obj$abbreviation))
      abbr <- as.character(obj$abbreviation)

    if (
      is.null(abbr) ||
      abbr != team
    )
      next

    for (nm in candidates) {

      if (!is.null(obj[[nm]])) {

        x <- as_num(obj[[nm]])

        if (
          length(x) &&
          is.finite(x[1]) &&
          x[1] >= 0
        )
          return(x[1])
      }
    }
  }

  # Do NOT silently reconstruct official yardage from our scored
  # play subset. Sacks, penalties and other NFL statistical rules
  # can make that differ from official net offense.

  NA_real_
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
  )
    stop(
      "Could not find gameDetail$plays for ",
      g
    )

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

  # Additional audit fields.

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
      as.character(p[[desc_col]])
    else
      rep("", nrow(p))

  deleted <-
    if (!is.na(deleted_col))
      as_flag(p[[deleted_col]])
    else
      rep(FALSE, nrow(p))

  special <-
    if (!is.na(st_col))
      as_flag(p[[st_col]])
    else
      rep(FALSE, nrow(p))

  # ----------------------------------------------------------
  # KEEP OFFENSIVE SCRIMMAGE PLAYS
  # ----------------------------------------------------------

  keep <-
    !deleted &
    !special &
    !is.na(team) &
    team != "" &
    !is.na(down) &
    down %in% 1:4 &
    !is.na(togo) &
    togo > 0 &
    !is.na(gain)

  no_play_text <- grepl(
    "No Play|NO PLAY",
    desc
  )

  keep <- keep & !no_play_text

  # ----------------------------------------------------------
  # AUDIT TABLE
  # ----------------------------------------------------------

  q <- data.frame(
    source_row = which(keep),
    team = team[keep],
    down = down[keep],
    yards_to_go = togo[keep],
    gain = gain[keep],
    description = desc[keep],
    stringsAsFactors = FALSE
  )

  q$play_id <-
    if (!is.na(play_id_col))
      as.character(p[[play_id_col]][keep])
    else
      as.character(q$source_row)

  q$drive <-
    if (!is.na(drive_col))
      as.character(p[[drive_col]][keep])
    else
      NA_character_

  q$quarter <-
    if (!is.na(quarter_col))
      as.character(p[[quarter_col]][keep])
    else
      NA_character_

  q$clock <-
    if (!is.na(clock_col))
      as.character(p[[clock_col]][keep])
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

  # Interceptions:
  # defensive return yards do not count toward offensive situation.

  q$gain[q$interception] <- 0

  q$gain_source <- "raw"

  q$gain_source[q$interception] <-
    "interception=0"

  # ----------------------------------------------------------
  # FUMBLE GAIN RECONSTRUCTION
  # ----------------------------------------------------------

  event_idx <- which(q$fumble)

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

      h <- tail(hits, 1)

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

        q$gain[i] <- val

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
    q$gain / q$target

  q$play_value <- mapply(
    play_value,
    q$down,
    q$yards_to_go,
    q$gain,
    q$turnover_event,
    q$kneel
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
    !is.finite(actual_away) ||
    !is.finite(actual_home)
  )
    stop(
      "Game does not appear complete: ",
      g
    )

  av <- sum(
    q$play_value[q$team == away],
    na.rm = TRUE
  )

  hv <- sum(
    q$play_value[q$team == home],
    na.rm = TRUE
  )

  ap <- sum(q$team == away)

  hp <- sum(q$team == home)

  # ----------------------------------------------------------
  # OFFICIAL YARDAGE
  # ----------------------------------------------------------

  away_yards <- get_team_yards(
    gd,
    away,
    p
  )

  home_yards <- get_team_yards(
    gd,
    home,
    p
  )

  if (
    !is.finite(away_yards) ||
    !is.finite(home_yards)
  ) {

    stop(
      "\nOfficial net offensive yardage could not be identified for ",
      g,
      ".\n",
      "NFL Numbers deliberately stops here rather than using ",
      "an unverified yardage reconstruction.\n"
    )
  }

  yard_away <-
    away_yards / 14.5

  yard_home <-
    home_yards / 14.5

  pool <-
    yard_away +
    yard_home

  if (
    !is.finite(av + hv) ||
    av + hv <= 0
  )
    stop(
      "Invalid situation-value pool for ",
      g
    )

  situ_away <-
    pool * av / (av + hv)

  situ_home <-
    pool * hv / (av + hv)

  overall_away <-
    0.30 * actual_away +
    0.30 * yard_away +
    0.40 * situ_away

  overall_home <-
    0.30 * actual_home +
    0.30 * yard_home +
    0.40 * situ_home

  # ----------------------------------------------------------
  # WRITE PLAY AUDIT
  # ----------------------------------------------------------

  play_file <- file.path(
    play_dir,
    paste0(g, "_exact_plays.csv")
  )

  write.csv(
    q,
    play_file,
    row.names = FALSE
  )

  # Existing compatibility location.

  write.csv(
    q,
    file.path(
      "csv",
      paste0(g, "_exact_plays.csv")
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

          data.frame(
            team = d$team[1],
            drive = d$drive[1],
            quarter = d$quarter[1],
            start_clock = d$clock[1],
            plays = nrow(d),
            scored_play_yards = sum(
              d$gain,
              na.rm = TRUE
            ),
            raw_situational = sum(
              d$play_value,
              na.rm = TRUE
            ),
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
        paste0(g, "_drives.csv")
      ),
      row.names = FALSE
    )
  }

  # ----------------------------------------------------------
  # MANUAL CHECK REPORT
  # ----------------------------------------------------------

  manual_n <- sum(
    q$gain_source == "MANUAL CHECK"
  )

  if (manual_n) {

    cat(
      "MANUAL CHECK plays for ",
      g,
      ": ",
      manual_n,
      "\n",
      sep = ""
    )

    print(
      q[
        q$gain_source == "MANUAL CHECK",
        c(
          "team",
          "down",
          "yards_to_go",
          "gain",
          "description"
        )
      ]
    )
  }

  cat(
    sprintf(
      paste0(
        "%-8s raw=%3d scored=%3d ",
        "(%s %d, %s %d) ",
        "values %.3f / %.3f ",
        "yards %.0f / %.0f\n"
      ),
      g,
      nrow(p),
      nrow(q),
      away,
      ap,
      home,
      hp,
      av,
      hv,
      away_yards,
      home_yards
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

    away_plays = ap,
    home_plays = hp,

    away_value = av,
    home_value = hv,

    actual_away = actual_away,
    actual_home = actual_home,

    official_yards_away = away_yards,
    official_yards_home = home_yards,

    yard_away = yard_away,
    yard_home = yard_home,

    situ_away = situ_away,
    situ_home = situ_home,

    overall_away = overall_away,
    overall_home = overall_home,

    quality = "A",

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

if (!length(rows))
  stop(
    "No completed games were successfully processed."
  )

res <- do.call(
  rbind,
  rows
)

rownames(res) <- NULL

# ============================================================
# VALIDATION
# ============================================================

if (any(!is.finite(res$overall_away)))
  stop("Non-finite away overall score.")

if (any(!is.finite(res$overall_home)))
  stop("Non-finite home overall score.")

# Situational totals MUST equal the game's yardage point pool.

situ_check <-
  res$situ_away +
  res$situ_home

yard_check <-
  res$yard_away +
  res$yard_home

if (
  any(
    abs(
      situ_check -
      yard_check
    ) > 1e-8
  )
)
  stop(
    "Situational pool reconciliation failed."
  )

# Overall must exactly reproduce 30/30/40.

oa_check <-
  0.30 * res$actual_away +
  0.30 * res$yard_away +
  0.40 * res$situ_away

oh_check <-
  0.30 * res$actual_home +
  0.30 * res$yard_home +
  0.40 * res$situ_home

if (
  any(
    abs(
      res$overall_away -
      oa_check
    ) > 1e-8
  ) ||
  any(
    abs(
      res$overall_home -
      oh_check
    ) > 1e-8
  )
)
  stop(
    "30/30/40 validation failed."
  )

# ============================================================
# OUTPUT
# ============================================================

results_file <- sprintf(
  "week%d_exact_results.csv",
  WEEK
)

js_file <- sprintf(
  "week%d_results.js",
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
    "games.csv"
  ),
  row.names = FALSE
)

js <- apply(
  res,
  1,
  function(z) {

    sprintf(
      paste0(
        '  {away:"%s",home:"%s",',
        'actual:[%.3f,%.3f],',
        'yard:[%.3f,%.3f],',
        'situ:[%.3f,%.3f],',
        'overall:[%.3f,%.3f],',
        'quality:"A"}'
      ),

      z["away"],
      z["home"],

      as.numeric(z["actual_away"]),
      as.numeric(z["actual_home"]),

      as.numeric(z["yard_away"]),
      as.numeric(z["yard_home"]),

      as.numeric(z["situ_away"]),
      as.numeric(z["situ_home"]),

      as.numeric(z["overall_away"]),
      as.numeric(z["overall_home"])
    )
  }
)

js_name <- paste0(
  "WEEK",
  WEEK,
  "_RESULTS"
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

writeLines(
  js_lines,
  js_file
)

writeLines(
  js_lines,
  file.path(
    week_dir,
    "results.js"
  )
)

# ============================================================
# SUMMARY
# ============================================================

cat("\n========================================\n")
cat("SUCCESS — NFL Numbers Week ", WEEK, "\n", sep = "")
cat("========================================\n")

cat(
  "Completed games processed: ",
  nrow(res),
  "\n",
  sep = ""
)

if (length(failed)) {

  cat(
    "Available games skipped: ",
    paste(failed, collapse = ", "),
    "\n",
    sep = ""
  )
}

print(
  res[
    ,
    c(
      "game",
      "actual_away",
      "actual_home",
      "official_yards_away",
      "official_yards_home",
      "situ_away",
      "situ_home",
      "overall_away",
      "overall_home",
      "away_plays",
      "home_plays"
    )
  ]
)
