
x <- readLines("convert_week.R", warn=FALSE)
txt <- paste(x, collapse="\n")

cal <- read.csv("locked_situation_calibration.csv", stringsAsFactors=FALSE)
if (nrow(cal) != 1 || !is.finite(cal$intercept[1]) || !is.finite(cal$slope[1])) {
  stop("Bad locked_situation_calibration.csv")
}

# Replace calibration constants with the just-derived full-2025 coefficients.
txt <- sub(
  "SITUATION_INTERCEPT <- [0-9.]+",
  sprintf("SITUATION_INTERCEPT <- %.12f", cal$intercept[1]),
  txt
)
txt <- sub(
  "SITUATION_SLOPE <- [0-9.]+",
  sprintf("SITUATION_SLOPE <- %.12f", cal$slope[1]),
  txt
)

# Replace canonical play_value block.
a <- regexpr("play_value <- function\\(", txt, perl=TRUE)[1]
bpat <- "# ------------------------------------------------------------\\n# PENALTY PARSER"
b <- regexpr(bpat, txt, perl=TRUE)[1]
if (a < 1 || b < 1 || b <= a) stop("Could not locate play_value block")

newfun <- paste(readLines("locked_short_yardage_patch.R", warn=FALSE), collapse="\n")
newfun <- sub("^.*?play_value <- function", "play_value <- function", newfun, perl=TRUE)

txt <- paste0(
  substr(txt,1,a-1),
  newfun, "\n\n",
  substr(txt,b,nchar(txt))
)

# Accepted penalty no-play: enforcement must not masquerade as football gain.
needle <- 'q$penalty_effect <- vapply('
pos <- regexpr(needle, txt, fixed=TRUE)[1]
if (pos < 1) stop("Could not locate penalty-effect block")

# Insert after the completed q$force_conversion vapply block, immediately before q$gain.
gainneedle <- '  q$gain <-\n    q$football_gain +'
gpos <- regexpr(gainneedle, txt, fixed=TRUE)[1]
if (gpos < 1) stop("Could not locate q$gain block")

correction <- paste0(
'  # LOCKED PENALTY CORRECTIONS\n',
'  no_play_now <- grepl("No Play|NO PLAY", q$description)\n',
'  q$football_gain[q$penalty_accepted & no_play_now] <- 0\n\n',
'  q$penalty_only_conversion <-\n',
'    q$down %in% c(3,4) &\n',
'    q$penalty_live &\n',
'    q$penalty_side == "defense" &\n',
'    (q$force_conversion | q$penalty_yards >= q$yards_to_go) &\n',
'    (no_play_now | q$football_gain < q$yards_to_go)\n\n',
'  q$force_conversion <- q$force_conversion | q$penalty_only_conversion\n\n'
)
txt <- paste0(substr(txt,1,gpos-1), correction, substr(txt,gpos,nchar(txt)))

# Add penalty_only_conversion as final mapply argument.
oldcall <- paste0(
'    q$turnover_event,\n',
'    q$kneel,\n',
'    q$force_conversion\n',
'  )'
)
newcall <- paste0(
'    q$turnover_event,\n',
'    q$kneel,\n',
'    q$force_conversion,\n',
'    q$penalty_only_conversion\n',
'  )'
)
if (!grepl(oldcall, txt, fixed=TRUE)) stop("Could not locate play_value mapply call")
txt <- sub(oldcall, newcall, txt, fixed=TRUE)

writeLines(strsplit(txt,"\n",fixed=TRUE)[[1]], "convert_week.R")
cat("Patched convert_week.R with locked short-yardage model and 2025 calibration.\n")
