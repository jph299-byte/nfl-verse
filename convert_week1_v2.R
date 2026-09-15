options(stringsAsFactors = FALSE)

# NFL Numbers — Week 1 exact situational fair-score converter
# Model:
#   target = 40% of yards-to-go on 1st, 60% on 2nd, 100% on 3rd/4th
#   failed 3rd/4th = max(-1.5, achievement - 1)
#   otherwise achievement <=1 = max(-1.5, achievement)
#   achievement >1 = min(1.75, 1 + 0.35*ln(achievement))
#   turnover penalty = -1.0; kneel = 0
# Overall fair score = 40% situation + 30% actual + 30% yardage
#
# Raw nflverse schema confirmed:
# x$data$viewer$gameDetail$plays

BASE_URL <- "https://github.com/nflverse/nflverse-pbp/releases/download/raw_pbp_2026"

GAMES <- c(
  "ARI_LAC","ATL_PIT","BAL_IND","BUF_HOU","CHI_CAR","CLE_JAX",
  "DAL_NYG","DEN_KC","GB_MIN","MIA_LV","NE_SEA","NO_DET",
  "NYJ_TEN","SF_LA","TB_CIN","WAS_PHI"
)

# Official team total net yards. DEN-KC added from the completed MNF box score.
OFFICIAL <- data.frame(
  game=GAMES,
  away_yards=c(393,238,506,409,552,272,244,176,419,259,277,469,367,379,282,295),
  home_yards=c(268,264,251,381,360,394,239,392,239,290,285,369,195,290,351,318),
  stringsAsFactors=FALSE
)

dir.create("rds", showWarnings=FALSE)
dir.create("csv", showWarnings=FALSE)

download_game <- function(g) {
  fn <- paste0("2026_01_",g,".rds")
  dest <- file.path("rds",fn)
  if (!file.exists(dest)) {
    url <- paste0(BASE_URL,"/",fn)
    cat("Downloading ",url,"\n",sep="")
    download.file(url,dest,mode="wb",quiet=FALSE)
  }
  if (!file.exists(dest) || file.info(dest)$size==0) stop("Bad download: ",g)
  dest
}

pick_col <- function(df, candidates, required=TRUE) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) return(hit[1])
  if (required) stop("Missing expected column. Tried: ",paste(candidates,collapse=", "))
  NA_character_
}

as_num <- function(x) suppressWarnings(as.numeric(x))
as_flag <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  tolower(as.character(x)) %in% c("true","1","yes")
}

play_value <- function(down, togo, gain, turnover=FALSE, kneel=FALSE) {
  if (isTRUE(kneel)) return(0)
  if (is.na(down)||is.na(togo)||is.na(gain)||togo<=0||!(down %in% 1:4)) return(NA_real_)
  target <- if (down==1) .4*togo else if (down==2) .6*togo else togo
  r <- gain/target
  if (down %in% c(3,4) && gain<togo) v <- max(-1.5,r-1)
  else if (r<=1) v <- max(-1.5,r)
  else v <- min(1.75,1+.35*log(r))
  if (isTRUE(turnover)) v <- v-1
  v
}

extract_game <- function(path, g) {
  x <- readRDS(path)
  gd <- x$data$viewer$gameDetail
  if (is.null(gd) || is.null(gd$plays) || !is.data.frame(gd$plays))
    stop("Could not find gameDetail$plays for ",g)
  p <- gd$plays

  team_col <- pick_col(p,c("possessionTeam.abbreviation","possessionTeamAbbreviation",
                           "possessionTeam","posteam"))
  down_col <- pick_col(p,c("down","downNumber"))
  togo_col <- pick_col(p,c("yardsToGo","ydstogo"))
  yards_col <- pick_col(p,c("yards","yardsGained","yards_gained"))
  desc_col <- pick_col(p,c("playDescription","shortDescription","desc"),FALSE)
  deleted_col <- pick_col(p,c("playDeleted","noPlay","no_play"),FALSE)
  st_col <- pick_col(p,c("specialTeamsPlay","specialTeams"),FALSE)

  team <- as.character(p[[team_col]])
  down <- as_num(p[[down_col]])
  togo <- as_num(p[[togo_col]])
  gain <- as_num(p[[yards_col]])
  desc <- if (!is.na(desc_col)) as.character(p[[desc_col]]) else rep("",nrow(p))
  deleted <- if (!is.na(deleted_col)) as_flag(p[[deleted_col]]) else rep(FALSE,nrow(p))
  special <- if (!is.na(st_col)) as_flag(p[[st_col]]) else rep(FALSE,nrow(p))

  # Only scrimmage plays with meaningful down/distance. This automatically
  # removes kickoffs, punts, PAT/FG attempts, timeouts and GAME markers.
  keep <- !deleted & !special & !is.na(team) & team!="" &
          !is.na(down) & down %in% 1:4 & !is.na(togo) & togo>0 & !is.na(gain)

  # Defensive penalties/no-plays can appear with down/distance but no football
  # play. Exclude explicit "No Play"; declined penalties remain because the play stands.
  no_play_text <- grepl("No Play|NO PLAY",desc)
  keep <- keep & !no_play_text

  q <- data.frame(
    team=team[keep], down=down[keep], yards_to_go=togo[keep],
    gain=gain[keep], description=desc[keep], stringsAsFactors=FALSE
  )

  # Event rules for NFL Numbers:
  # - every genuine fumble costs -1, whether recovered by offense/defense/OOB;
  # - every interception costs -1;
  # - the yards component must stop at the turnover/fumble, so return yards
  #   are never credited/debited to the offense.
  q$interception <- grepl("INTERCEPTED|intercepted by",q$description,
                          ignore.case=TRUE)
  q$fumble <- grepl("FUMBLES|Fumble",q$description,ignore.case=TRUE)

  # If replay explicitly reverses the fumble/interception, do not apply -1.
  reversed <- grepl("reversed|overturned|was down by contact|ruling.*changed",
                    q$description,ignore.case=TRUE)
  q$interception[reversed] <- FALSE
  q$fumble[reversed] <- FALSE

  # Interceptions are incomplete offensive passes for our yardage-of-the-play
  # measure: return yards belong to the defense, not the offense.
  q$gain[q$interception] <- 0

  # For fumbles, recover the offensive gain BEFORE the fumble from the first
  # "for N yards" statement before the word FUMBLE. If unavailable, preserve
  # the raw gain but flag it in the audit CSV for manual review.
  q$gain_source <- "raw"
  fidx <- which(q$fumble)
  for (i in fidx) {
    pre <- sub("(?i)FUMBLES?.*$","",q$description[i],perl=TRUE)
    mm <- gregexpr("for (-?[0-9]+) yards?",pre,perl=TRUE,ignore.case=TRUE)
    hits <- regmatches(pre,mm)[[1]]
    if (length(hits) && hits[1] != "-1") {
      nums <- as.numeric(sub(".*for (-?[0-9]+) yards?.*","\\1",hits,
                             perl=TRUE,ignore.case=TRUE))
      nums <- nums[is.finite(nums)]
      if (length(nums)) {
        q$gain[i] <- tail(nums,1)
        q$gain_source[i] <- "pre-fumble description"
      }
    } else if (grepl("sacked .* for -?[0-9]+ yards",pre,ignore.case=TRUE)) {
      z <- sub(".*sacked .* for (-?[0-9]+) yards.*","\\1",pre,
               perl=TRUE,ignore.case=TRUE)
      q$gain[i] <- as.numeric(z)
      q$gain_source[i] <- "pre-fumble sack"
    } else if (grepl("FUMBLES \\(Aborted\\)",q$description[i],ignore.case=TRUE)) {
      # Aborted exchanges are negative/zero plays. Raw NFL gain is normally
      # already the correct offensive result up to recovery; retain and flag.
      q$gain_source[i] <- "raw-aborted"
    } else {
      q$gain_source[i] <- "MANUAL CHECK"
    }
  }

  q$turnover_event <- q$interception | q$fumble
  q$kneel <- grepl("kneel|kneels",q$description,ignore.case=TRUE)
  q$target <- ifelse(q$down==1,.4*q$yards_to_go,
                     ifelse(q$down==2,.6*q$yards_to_go,q$yards_to_go))
  q$achievement <- q$gain/q$target
  q$play_value <- mapply(play_value,q$down,q$yards_to_go,q$gain,q$turnover_event,q$kneel)

  away <- gd$visitorTeam$abbreviation
  home <- gd$homeTeam$abbreviation
  actual_away <- as.numeric(gd$visitorPointsTotal)
  actual_home <- as.numeric(gd$homePointsTotal)

  # Standardize only for app display.
  display_team <- function(z) {
    z[z=="LA"] <- "LAR"; z[z=="JAX"] <- "JAC"; z
  }

  # Keep source abbreviations for matching raw plays.
  av <- sum(q$play_value[q$team==away],na.rm=TRUE)
  hv <- sum(q$play_value[q$team==home],na.rm=TRUE)
  ap <- sum(q$team==away); hp <- sum(q$team==home)

  off <- OFFICIAL[OFFICIAL$game==g,]
  yard_away <- off$away_yards/14.5
  yard_home <- off$home_yards/14.5
  pool <- yard_away+yard_home
  if (!is.finite(av+hv) || av+hv<=0) stop("Invalid situation-value pool for ",g)
  situ_away <- pool*av/(av+hv)
  situ_home <- pool*hv/(av+hv)
  overall_away <- .30*actual_away+.30*yard_away+.40*situ_away
  overall_home <- .30*actual_home+.30*yard_home+.40*situ_home

  # Audit file: every scored offensive snap.
  write.csv(q,file.path("csv",paste0(g,"_exact_plays.csv")),row.names=FALSE)

  manual_n <- sum(q$gain_source=="MANUAL CHECK")
  if (manual_n) {
    cat("MANUAL CHECK plays for ",g,": ",manual_n,"\n",sep="")
    print(q[q$gain_source=="MANUAL CHECK",
            c("team","down","yards_to_go","gain","description")])
  }
  cat(sprintf("%-8s raw plays=%3d scored offense=%3d (%s %d, %s %d) values %.3f / %.3f\n",
              g,nrow(p),nrow(q),away,ap,home,hp,av,hv))

  data.frame(
    game=g, away=display_team(away), home=display_team(home),
    away_plays=ap,home_plays=hp,away_value=av,home_value=hv,
    actual_away=actual_away,actual_home=actual_home,
    official_yards_away=off$away_yards,official_yards_home=off$home_yards,
    yard_away=yard_away,yard_home=yard_home,
    situ_away=situ_away,situ_home=situ_home,
    overall_away=overall_away,overall_home=overall_home,
    quality="A",stringsAsFactors=FALSE
  )
}

rows <- list()
for (g in GAMES) {
  cat("\n=== ",g," ===\n",sep="")
  rows[[g]] <- extract_game(download_game(g),g)
}
res <- do.call(rbind,rows)
rownames(res) <- NULL

# Validation: game scores are read directly from nflverse. DEN-KC should be 10-31.
stopifnot(nrow(res)==16)
denkc <- res[res$game=="DEN_KC",]
stopifnot(nrow(denkc)==1, denkc$actual_away==10, denkc$actual_home==31)

write.csv(res,"week1_exact_results.csv",row.names=FALSE)

js <- apply(res,1,function(z) sprintf(
  '  {away:"%s",home:"%s",actual:[%.3f,%.3f],yard:[%.3f,%.3f],situ:[%.3f,%.3f],overall:[%.3f,%.3f],quality:"A"}',
  z["away"],z["home"],as.numeric(z["actual_away"]),as.numeric(z["actual_home"]),
  as.numeric(z["yard_away"]),as.numeric(z["yard_home"]),
  as.numeric(z["situ_away"]),as.numeric(z["situ_home"]),
  as.numeric(z["overall_away"]),as.numeric(z["overall_home"])
))
writeLines(c("const WEEK1_RESULTS = [",paste(js,collapse=",\n"),"];"),
           "week1_results.js")

cat("\nSUCCESS: exact Week 1 files created for all 16 games.\n")
cat("DEN-KC official result/yardage: DEN 10, 176 yards; KC 31, 392 yards.\n")
print(res[,c("game","actual_away","actual_home","situ_away","situ_home",
             "overall_away","overall_home","away_plays","home_plays")])
