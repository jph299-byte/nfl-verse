# NFL Week 1 exact play-by-play converter

This package converts the nflverse 2026 Week 1 `.rds` game files to CSV and applies the exact situational-play formula agreed for the NFL Numbers model.

## Formula
- 1st down target = 40% of yards to go
- 2nd down target = 60%
- 3rd/4th down target = full conversion
- failed 3rd/4th: `max(-1.5, r - 1)`
- otherwise if `r <= 1`: `max(-1.5, r)`
- if `r > 1`: `min(1.75, 1 + 0.35*ln(r))`
- turnover: subtract 1.0
- kneel: 0
- no-play/nullified plays excluded

Game situation points are allocated from the same yardage-score pool:
`(away official yards + home official yards) / 14.5`

Overall fair score:
`30% actual + 30% yardage + 40% situation`

## Outputs
- `week1_exact_results.csv`
- `week1_results.js`
- `csv/*.csv` snap-level converted game files

## GitHub Actions
Put these files in any writable GitHub repository, then run the **Convert Week 1 NFL PBP** workflow manually. It downloads the public nflverse release files itself and uploads the exact outputs as a workflow artifact.
