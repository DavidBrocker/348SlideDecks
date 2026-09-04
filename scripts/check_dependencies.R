#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# check_dependencies.R
#
# Scans every .qmd / .R file in the repo for R package dependencies --
# explicit (library(pkg), require(pkg)), namespace-qualified (pkg::fn()),
# and a curated list of "invisible" dependencies: packages pulled in
# behind the scenes by a function call without ever being named directly
# (e.g. gt::gtsave(*.png) silently needs webshot2 + chromote).
#
# Run this from the repo root -- e.g. right after opening a fresh Posit
# Cloud workspace -- to see everything that's missing before Quarto
# discovers it one broken chunk at a time.
#
#   Rscript scripts/check_dependencies.R            # report only
#   Rscript scripts/check_dependencies.R --install   # also install missing
#
# Base R only (no package dependencies of its own), so it runs before
# anything else is installed.
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
do_install <- "--install" %in% args

root <- normalizePath(".")

# Directories to skip -- build output / cache / renv library, not source
skip_dirs <- c("_freeze", "/docs/", "renv", "site_libs", "unused_assets")

files <- list.files(root, pattern = "\\.(qmd|R|r)$", recursive = TRUE, full.names = TRUE)
files <- files[!grepl(paste(skip_dirs, collapse = "|"), files, fixed = FALSE)]

# Don't scan this script itself -- its own comments contain example
# patterns like "pkg::fn()" and "gt::gtsave()" that would otherwise be
# misread as real dependencies.
this_file <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)),
                            mustWork = FALSE)
if (length(this_file) == 1 && nzchar(this_file)) {
  files <- files[normalizePath(files, mustWork = FALSE) != this_file]
}

read_file <- function(f) tryCatch(paste(readLines(f, warn = FALSE), collapse = "\n"),
                                   error = function(e) "")
all_text <- vapply(files, read_file, character(1))
blob <- paste(all_text, collapse = "\n")

# --- explicit library()/require() calls ---
explicit <- unique(unlist(regmatches(
  blob, gregexpr("(?<=library\\(|require\\()\\s*[A-Za-z0-9\\.]+", blob, perl = TRUE)
)))
explicit <- trimws(explicit)

# --- namespace-qualified pkg::fn() usage ---
ns <- unique(unlist(regmatches(
  blob, gregexpr("\\b[A-Za-z][A-Za-z0-9\\.]*(?=::)", blob, perl = TRUE)
)))

# --- curated "invisible dependency" triggers ---
# regex pattern (searched across all source text) -> package(s) it silently
# requires at render time. Add to this list whenever a render fails on a
# package that was never named directly in the .qmd.
implicit_triggers <- list(
  "gtsave\\([^)]*\\.(png|pdf|jpe?g)" = c("webshot2", "chromote"),
  "gganimate::|anim_save\\("         = c("gifski"),
  "showtext"                          = c("sysfonts", "showtextdb"),
  "ggiraph"                           = c("systemfonts")
)

implicit <- character(0)
for (pat in names(implicit_triggers)) {
  if (grepl(pat, blob, perl = TRUE)) implicit <- c(implicit, implicit_triggers[[pat]])
}
implicit <- unique(implicit)

needed <- sort(unique(c(explicit, ns, implicit, "tidyverse", "quarto")))

installed <- rownames(installed.packages())
missing <- setdiff(needed, installed)

cat("348SlideDecks dependency check\n")
cat("================================\n")
cat(sprintf("Scanned %d files, found %d referenced packages.\n\n", length(files), length(needed)))

if (length(missing) == 0) {
  cat("All detected packages are installed.\n")
} else {
  cat("Missing packages:\n")
  cat(paste0("  - ", missing, collapse = "\n"), "\n\n")
  cmd <- sprintf("install.packages(c(%s))", paste(sprintf('"%s"', missing), collapse = ", "))
  cat("Install with:\n  ", cmd, "\n\n", sep = "")
  if (do_install) {
    cat("Installing now...\n")
    install.packages(missing)
  }
}

cat("\nNote: this scan can't see every implicit dependency -- only the ones\n")
cat("in `implicit_triggers` above. If Quarto still complains about a\n")
cat("missing package after this passes clean, add its trigger pattern to\n")
cat("that list so future runs (and future Dave) catch it automatically.\n")
