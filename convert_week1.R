options(stringsAsFactors = FALSE)

# NFL Numbers — Week 1 nflverse raw-PBP diagnostic
# Clean standalone file. This first inspects the real nflverse RDS schema so
# the exact 16-game scorer can be built against the data rather than guessed.

BASE_URL <- "https://github.com/nflverse/nflverse-pbp/releases/download/raw_pbp_2026"

GAMES <- c(
  "ARI_LAC", "ATL_PIT", "BAL_IND", "BUF_HOU",
  "CHI_CAR", "CLE_JAX", "DAL_NYG", "DEN_KC",
  "GB_MIN", "MIA_LV", "NE_SEA", "NO_DET",
  "NYJ_TEN", "SF_LA", "TB_CIN", "WAS_PHI"
)

dir.create("rds", showWarnings = FALSE)

download_game <- function(game) {
  filename <- paste0("2026_01_", game, ".rds")
  destination <- file.path("rds", filename)
  url <- paste0(BASE_URL, "/", filename)

  cat("Downloading: ", url, "\n", sep = "")
  download.file(url, destination, mode = "wb", quiet = FALSE)

  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop("Download failed: ", filename)
  }

  cat("Saved: ", destination, " (", file.info(destination)$size, " bytes)\n",
      sep = "")
  destination
}

inspect_rds <- function(path) {
  x <- readRDS(path)

  cat("\n")
  cat("========== NFLVERSE RAW RDS DIAGNOSTIC ==========\n")
  cat("File: ", path, "\n", sep = "")
  cat("Class: ", paste(class(x), collapse = ", "), "\n", sep = "")
  cat("Type: ", typeof(x), "\n", sep = "")
  cat("Length: ", length(x), "\n", sep = "")

  if (is.null(dim(x))) {
    cat("Dimensions: <none>\n")
  } else {
    cat("Dimensions: ", paste(dim(x), collapse = " x "), "\n", sep = "")
  }

  if (!is.null(names(x))) {
    cat("\nTop-level names:\n")
    print(head(names(x), 100))
  }

  cat("\nTop-level structure:\n")
  str(x, max.level = 5, list.len = 100, give.attr = TRUE)

  if (is.list(x) && length(x) > 0) {
    cat("\nFirst element class: ",
        paste(class(x[[1]]), collapse = ", "), "\n", sep = "")
    cat("First element type: ", typeof(x[[1]]), "\n", sep = "")

    if (!is.null(names(x[[1]]))) {
      cat("\nFirst element names:\n")
      print(head(names(x[[1]]), 100))
    }

    cat("\nFirst element structure:\n")
    str(x[[1]], max.level = 5, list.len = 100, give.attr = TRUE)
  }

  cat("\n========== END NFLVERSE RAW RDS DIAGNOSTIC ==========\n")
  invisible(x)
}

# Inspect ARI-LAC only. Once its schema is known, the same parser can be used
# across all Week 1 games. DEN-KC is already included in GAMES above.
first_path <- download_game(GAMES[1])
inspect_rds(first_path)

cat("\nDiagnostic completed successfully.\n")
cat("Week 1 game list contains ", length(GAMES), " games, including DEN-KC.\n",
    sep = "")
