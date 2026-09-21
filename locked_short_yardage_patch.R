# NFL Numbers — locked short-yardage model patch
# Applies to convert_week.R production engine.
#
# LOCKED:
# - successful 2nd down with 1-4 yards to go: +1 conversion, /8 excess-yard curve
# - successful 3rd/4th down: same
# - cap 1.75
# - failed plays retain existing treatment
# - penalty-only defensive 3rd/4th conversion = exactly +1.00
#
# This file is intentionally a complete replacement helper for the scoring function.

play_value <- function(
  down,
  togo,
  gain,
  turnover = FALSE,
  kneel = FALSE,
  force_conversion = FALSE,
  penalty_only_conversion = FALSE
) {
  if (isTRUE(kneel)) return(0)

  if (is.na(down) || is.na(togo) || is.na(gain) ||
      togo <= 0 || !(down %in% 1:4)) return(NA_real_)

  # Penalty itself supplies the 3rd/4th-down conversion:
  # minimum conversion credit only; enforcement distance is not big-play yardage.
  if (isTRUE(penalty_only_conversion) && down %in% c(3,4)) {
    v <- 1.00
    if (isTRUE(turnover)) v <- v - 1
    return(v)
  }

  # Locked conversion/excess rule.
  conversion_curve <-
    (down %in% c(3,4) && (gain >= togo || isTRUE(force_conversion))) ||
    (down == 2 && togo <= 4 && gain >= togo)

  if (conversion_curve) {
    # Only genuine football yardage beyond the line to gain earns excess credit.
    # For forced penalty conversions with insufficient football gain, excess = 0.
    excess <- max(0, gain - togo)
    if (isTRUE(force_conversion) && gain < togo) excess <- 0
    v <- min(1.75, 1 + 0.35 * log(1 + excess / 8))
  } else {
    target <- if (down == 1) 0.4*togo else if (down == 2) 0.6*togo else togo
    r <- gain / target
    if (down %in% c(3,4) && gain < togo) {
      v <- max(-1.5, r - 1)
    } else if (r <= 1) {
      v <- max(-1.5, r)
    } else {
      v <- min(1.75, 1 + 0.35 * log(r))
    }
  }

  if (isTRUE(turnover)) v <- v - 1
  v
}
