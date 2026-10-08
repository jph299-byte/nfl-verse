# Generate canonical old/new 2025 postseason Situation observations.
# Uses exact scoring and team_games functions from canonical_situation_v3.R.
suppressPackageStartupMessages({library(nflreadr);library(dplyr);library(stringr)})
source_file <- 'canonical_situation_v3.R'
stopifnot(file.exists(source_file))
src <- readLines(source_file, warn=FALSE)
cut <- which(src == 'p25 <- prepare(2025)')
stopifnot(length(cut)==1)
old <- '  target <- if (d==1) .4*togo else if (d==2) .6*togo else togo'
stopifnot(sum(src==old)==1)
for (variant in c('current_60','proposed_100_70')) {
  s <- src[seq_len(cut-1)]
  if (variant=='proposed_100_70') s[s==old] <- '  target <- if (d==1) .4*togo else if (d==2) if (togo<=4) togo else .7*togo else togo'
  s <- gsub('filter(season_type=="REG")', 'filter(season_type %in% c("REG","POST"))',s,fixed=TRUE)
  s <- gsub('if (season==2025) p <- p |> filter(week<=18)', 'if (season==2025) p <- p |> filter(week<=22)',s,fixed=TRUE)
  eval(parse(text=paste(s,collapse='\n')),envir=.GlobalEnv)
  p <- prepare(2025) |> filter(season_type=='POST')
  tg <- team_games(p)
  dir.create('second_down_replay_outputs',showWarnings=FALSE)
  write.csv(tg,file.path('second_down_replay_outputs',paste0('postseason_',variant,'.csv')),row.names=FALSE)
  cat(variant,':',nrow(tg),'postseason team-games\n')
  stopifnot(nrow(tg)==26)
}
