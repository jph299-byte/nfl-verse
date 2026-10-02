# Corrected pure-Yardage sequential replay.
# Uses ONLY the already-verified pure-yardage CSV plus recovered pre-W1 2025 ratings.
# Base R only: no nflreadr or other packages.

flatten <- 0.40
lg_pass <- 234
lg_rush <- 121

lr_for <- function(week) {
  if (week == 1) 0.20 else if (week == 2) 0.175 else 0.15
}

ratings <- read.csv("pre_week1_2025_yardage_ratings.csv", stringsAsFactors=FALSE)
obs <- read.csv("pure_yardage_inputs_2025_2026w1-3.csv", stringsAsFactors=FALSE)

stopifnot(nrow(ratings) == 32, length(unique(ratings$team)) == 32)
# This stage is 2025 only. 2026 is deliberately retained in the source CSV for later.
obs <- obs[obs$season == 2025, ]
stopifnot(nrow(obs) == 570)

# nflverse uses LA; recovered app ratings use LAR.
obs$posteam[obs$posteam == "LA"] <- "LAR"

# Derive opponent from the two team rows for each game, avoiding assumptions about game_id parsing.
obs$opponent <- NA_character_
for (gid in unique(obs$game_id)) {
  ix <- which(obs$game_id == gid)
  if (length(ix) != 2) stop("Expected exactly two team rows for ", gid)
  obs$opponent[ix[1]] <- obs$posteam[ix[2]]
  obs$opponent[ix[2]] <- obs$posteam[ix[1]]
}
if (!all(obs$posteam %in% ratings$team) || !all(obs$opponent %in% ratings$team))
  stop("Team-code mismatch between observations and recovered ratings")

initial <- ratings
write.csv(initial, "recovered_pre_week1_2025_ratings_used.csv", row.names=FALSE)

expect_team <- function(team, opp, r) {
  t <- r[r$team == team, ]
  o <- r[r$team == opp, ]
  c(
    exp_pass = t$off_pass * o$def_pass / lg_pass,
    exp_rush = t$off_rush * o$def_rush / lg_rush
  )
}

audit <- list()
snapshots <- list()
ai <- 1
si <- 1

# IMPORTANT: freeze ratings at the start of each week.
# Every game in a week uses the same entering-week ratings; updates are applied only after
# all games in that week have been evaluated.
week_keys <- unique(obs[, c("season_type","week")])
week_keys$type_order <- ifelse(week_keys$season_type == "REG", 0, 1)
week_keys <- week_keys[order(week_keys$type_order, week_keys$week), ]

for (wkrow in seq_len(nrow(week_keys))) {
  st <- week_keys$season_type[wkrow]
  wk <- week_keys$week[wkrow]
  wobs <- obs[obs$season_type == st & obs$week == wk, ]
  entering <- ratings
  updates <- data.frame(team=ratings$team, off_pass=0, off_rush=0, def_pass=0, def_rush=0)
  lr <- lr_for(wk)

  for (j in seq_len(nrow(wobs))) {
    x <- wobs[j, ]
    e <- expect_team(x$posteam, x$opponent, entering)
    total <- x$adjusted_offensive_yards
    raw_share <- if (total > 0) x$adjusted_pass_yards / total else e["exp_pass"] / sum(e)
    exp_share <- e["exp_pass"] / sum(e)
    learned_share <- (1 - flatten) * raw_share + flatten * exp_share
    learned_pass <- total * learned_share
    learned_rush <- total - learned_pass
    dpass <- lr * (learned_pass - e["exp_pass"])
    drush <- lr * (learned_rush - e["exp_rush"])

    audit[[ai]] <- data.frame(
      game_id=x$game_id, season_type=st, week=wk,
      team=x$posteam, opponent=x$opponent,
      net_pass=x$official_net_pass_yards,
      rush=x$official_rush_yards,
      dpi_yards=x$accepted_defensive_dpi_yards,
      dpi_credit=x$dpi_credit,
      adj_pass=x$adjusted_pass_yards,
      adj_total=total,
      exp_pass=e["exp_pass"], exp_rush=e["exp_rush"],
      raw_pass_share=raw_share, expected_pass_share=exp_share,
      learned_pass_share=learned_share,
      learned_pass=learned_pass, learned_rush=learned_rush,
      dpass=dpass, drush=drush, learning_rate=lr,
      row.names=NULL
    )
    ai <- ai + 1

    ti <- match(x$posteam, updates$team)
    oi <- match(x$opponent, updates$team)
    updates$off_pass[ti] <- updates$off_pass[ti] + dpass
    updates$off_rush[ti] <- updates$off_rush[ti] + drush
    updates$def_pass[oi] <- updates$def_pass[oi] + dpass
    updates$def_rush[oi] <- updates$def_rush[oi] + drush
  }

  # Apply all weekly changes simultaneously.
  ratings$off_pass <- ratings$off_pass + updates$off_pass
  ratings$off_rush <- ratings$off_rush + updates$off_rush
  ratings$def_pass <- ratings$def_pass + updates$def_pass
  ratings$def_rush <- ratings$def_rush + updates$def_rush

  snap <- ratings
  snap$season_type <- st
  snap$week <- wk
  snapshots[[si]] <- snap
  si <- si + 1
}

audit <- do.call(rbind, audit)
snapshots <- do.call(rbind, snapshots)

# Hard checks.
if (nrow(audit) != 570) stop("2025 team-game count failed")
err <- max(abs((audit$learned_pass + audit$learned_rush) - audit$adj_total))
if (err > 1e-8) stop("Total-yard preservation failed: ", err)
share_err <- max(abs(audit$learned_pass_share -
  (0.60 * audit$raw_pass_share + 0.40 * audit$expected_pass_share)))
if (share_err > 1e-10) stop("40% split-regression identity failed: ", share_err)
if (any(!is.finite(as.matrix(ratings[,c("off_pass","off_rush","def_pass","def_rush")]))))
  stop("Non-finite final rating")

write.csv(audit, "corrected_2025_yardage_replay_audit.csv", row.names=FALSE)
write.csv(ratings, "corrected_post_2025_yardage_ratings.csv", row.names=FALSE)
write.csv(snapshots, "corrected_2025_yardage_rating_snapshots.csv", row.names=FALSE)

writeLines(c(
  "Corrected 2025 Yardage replay",
  "Starting state: exact pre-Week-1 2025 ratings recovered from first retained deploy.",
  "Input observations: previously verified pure-yardage CSV; no PBP reconstruction in this workflow.",
  "No nflreadr and no external R packages.",
  "Entering league baseline: pass 234, rush 121.",
  "50% accepted defensive DPI credit is already present in the verified input CSV.",
  "Learned pass share = 60% observed adjusted share + 40% pre-game expected share.",
  "Learned pass + learned rush preserves observed adjusted total exactly.",
  "Ratings are frozen at the start of each week; all weekly updates are simultaneous.",
  "Learning: W1 20%, W2 17.5%, W3+ 15%.",
  "This stage stops after the 2025 postseason; no 2026 offseason bridge is invented.",
  paste("Team-games:", nrow(audit)),
  paste("Max total-preservation error:", format(err, scientific=TRUE))
), "corrected_2025_yardage_replay_summary.txt")
