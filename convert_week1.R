
options(stringsAsFactors = FALSE)

base_url <- "https://github.com/nflverse/nflverse-pbp/releases/download/raw_pbp_2026"

games <- c(
  "ARI_LAC","ATL_PIT","BAL_IND","BUF_HOU","CHI_CAR",
  "CLE_JAX","DAL_NYG","GB_MIN","MIA_LV","NE_SEA",
  "NO_DET","NYJ_TEN","SF_LA","TB_CIN","WAS_PHI"
)

dir.create("rds", showWarnings = FALSE)
dir.create("csv", showWarnings = FALSE)

download_game <- function(g) {
  fn <- paste0("2026_01_", g, ".rds")
  dest <- file.path("rds", fn)
  if (!file.exists(dest)) {
    download.file(paste0(base_url, "/", fn), dest, mode="wb", quiet=FALSE)
  }
  dest
}

normalise_team <- function(x) {
  x[x=="JAX"] <- "JAC"
  x[x=="LA"] <- "LAR"
  x
}

play_value <- function(down, ydstogo, gain, turnover=FALSE, kneel=FALSE) {
  if (isTRUE(kneel)) return(0)
  if (is.na(down) || is.na(ydstogo) || is.na(gain) || ydstogo <= 0) return(NA_real_)

  target <- if (down == 1) 0.4*ydstogo else if (down == 2) 0.6*ydstogo else ydstogo
  r <- gain / target

  if (down %in% c(3,4) && gain < ydstogo) {
    v <- max(-1.5, r - 1)
  } else if (r <= 1) {
    v <- max(-1.5, r)
  } else {
    v <- min(1.75, 1 + 0.35*log(r))
  }

  if (isTRUE(turnover)) v <- v - 1
  v
}

extract_game <- function(path) {
  x <- readRDS(path)

  # nflverse files are data frames/tibbles at this stage.
  if (!is.data.frame(x)) stop("Unexpected RDS structure: ", path)

  needed <- c("game_id","posteam","down","ydstogo","yards_gained","desc")
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("Missing columns: ", paste(missing, collapse=", "))

  if (!"play_type" %in% names(x)) x$play_type <- NA_character_
  if (!"qb_kneel" %in% names(x)) x$qb_kneel <- 0
  if (!"fumble_lost" %in% names(x)) x$fumble_lost <- 0
  if (!"interception" %in% names(x)) x$interception <- 0
  if (!"no_play" %in% names(x)) x$no_play <- 0

  # Offensive snaps only. Exclude nullified/no-play penalties.
  y <- x[
    !is.na(x$posteam) &
    !is.na(x$down) &
    !is.na(x$ydstogo) &
    !is.na(x$yards_gained) &
    (is.na(x$no_play) | x$no_play == 0),
  ]

  # Keep rush/pass/sack/kneel and other genuine scrimmage plays with yardage.
  # Drop kick/punt/timeout/admin rows if play_type is populated.
  non_off <- y$play_type %in% c(
    "kickoff","field_goal","extra_point","punt","no_play",
    "qb_spike","timeout","end_game","end_quarter"
  )
  non_off[is.na(non_off)] <- FALSE
  y <- y[!non_off,]

  y$posteam <- normalise_team(as.character(y$posteam))
  y$turnover <- (ifelse(is.na(y$fumble_lost),0,y$fumble_lost) == 1) |
                (ifelse(is.na(y$interception),0,y$interception) == 1)
  y$kneel <- ifelse(is.na(y$qb_kneel),0,y$qb_kneel) == 1

  y$target <- ifelse(y$down==1, 0.4*y$ydstogo,
              ifelse(y$down==2, 0.6*y$ydstogo, y$ydstogo))
  y$achievement <- y$yards_gained / y$target
  y$play_value <- mapply(
    play_value,
    y$down, y$ydstogo, y$yards_gained, y$turnover, y$kneel
  )

  y
}

official_yards <- data.frame(
  away=c("ARI","ATL","BAL","BUF","CHI","CLE","DAL","GB","MIA","NE","NO","NYJ","SF","TB","WAS"),
  home=c("LAC","PIT","IND","HOU","CAR","JAC","NYG","MIN","LV","SEA","DET","TEN","LAR","CIN","PHI"),
  away_yards=c(393,238,506,409,552,272,244,419,259,277,469,367,379,282,295),
  home_yards=c(268,264,251,381,478,360,394,239,290,285,369,195,290,351,318),
  stringsAsFactors=FALSE
)

actual <- data.frame(
  away=c("ARI","ATL","BAL","BUF","CHI","CLE","DAL","GB","MIA","NE","NO","NYJ","SF","TB","WAS"),
  home=c("LAC","PIT","IND","HOU","CAR","JAC","NYG","MIN","LV","SEA","DET","TEN","LAR","CIN","PHI"),
  away_pts=c(26,13,41,36,59,10,20,22,13,10,30,23,27,27,22),
  home_pts=c(14,20,23,31,37,34,28,39,27,13,31,10,7,33,24),
  stringsAsFactors=FALSE
)

summary_rows <- list()

for (g in games) {
  path <- download_game(g)
  y <- extract_game(path)
  write.csv(y, file.path("csv", sub("\\.rds$", ".csv", basename(path))), row.names=FALSE)

  teams <- strsplit(g, "_", fixed=TRUE)[[1]]
  a <- normalise_team(teams[1]); h <- normalise_team(teams[2])

  vals <- aggregate(play_value ~ posteam, data=y, sum, na.rm=TRUE)
  plays <- aggregate(play_value ~ posteam, data=y, length)
  names(plays)[2] <- "plays"
  vals <- merge(vals, plays, by="posteam")

  av <- vals$play_value[vals$posteam==a]
  hv <- vals$play_value[vals$posteam==h]
  ap <- vals$plays[vals$posteam==a]
  hp <- vals$plays[vals$posteam==h]

  oy <- official_yards[official_yards$away==a & official_yards$home==h,]
  ac <- actual[actual$away==a & actual$home==h,]

  yard_away <- oy$away_yards / 14.5
  yard_home <- oy$home_yards / 14.5
  pool <- yard_away + yard_home

  situ_away <- pool * av / (av + hv)
  situ_home <- pool * hv / (av + hv)

  overall_away <- 0.30*ac$away_pts + 0.30*yard_away + 0.40*situ_away
  overall_home <- 0.30*ac$home_pts + 0.30*yard_home + 0.40*situ_home

  summary_rows[[length(summary_rows)+1]] <- data.frame(
    away=a,home=h,
    away_plays=ap,home_plays=hp,
    away_value=av,home_value=hv,
    actual_away=ac$away_pts,actual_home=ac$home_pts,
    yard_away=yard_away,yard_home=yard_home,
    situ_away=situ_away,situ_home=situ_home,
    overall_away=overall_away,overall_home=overall_home,
    quality="A",
    stringsAsFactors=FALSE
  )
}

res <- do.call(rbind, summary_rows)
write.csv(res, "week1_exact_results.csv", row.names=FALSE)

# JS-ready payload for direct app replacement
payload <- apply(res,1,function(z) {
  sprintf(
    '{away:"%s",home:"%s",actual:[%.3f,%.3f],yard:[%.3f,%.3f],situ:[%.3f,%.3f],quality:"A"}',
    z["away"],z["home"],
    as.numeric(z["actual_away"]),as.numeric(z["actual_home"]),
    as.numeric(z["yard_away"]),as.numeric(z["yard_home"]),
    as.numeric(z["situ_away"]),as.numeric(z["situ_home"])
  )
})
writeLines(c("const WEEK1_RESULTS = [", paste0("  ",payload,collapse=",\n"), "];"),
           "week1_results.js")

cat("\nDone.\nCreated:\n",
    "  week1_exact_results.csv\n",
    "  week1_results.js\n",
    "  csv/*.csv (snap-level game files)\n")
