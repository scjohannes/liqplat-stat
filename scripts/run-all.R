# Render all LIQPLAT analysis notebooks in dependency order.
#
#   Rscript scripts/run-all.R                              # everything
#   Rscript scripts/run-all.R --from analysis/04-taooh     # from the first match on
#   Rscript scripts/run-all.R --only analysis/09-imaging   # only matching notebooks
#
# --from and --only take a path prefix. Notebooks inside a folder run in file
# name order; the order of the folders below encodes the dependencies between
# analyses. Imputations and model fits are cached in each analysis's results/
# folder: delete that folder to recompute them.

if (getRversion() != "4.6.1") {
  stop("LIQPLAT requires R 4.6.1; this is R ", getRversion(), ".")
}

library(here)

run_order <- c(
  "analysis/00-data",
  "analysis/01-population",

  # Overall survival, main model first: the QoL analysis decision needs its
  # RMST. The 26-week death analysis needs the QoL vital-status model, so it
  # runs after the QoL landmark imputation.
  "analysis/02-overall-survival/01-main",
  "analysis/02-overall-survival/02-non-proportional-hazards",
  "analysis/03-quality-of-life/00-analysis-decision.qmd",
  "analysis/03-quality-of-life/01-preparation.qmd",
  "analysis/03-quality-of-life/02-six-month-landmark",
  "analysis/02-overall-survival/03-death-at-26-weeks",
  "analysis/03-quality-of-life/03-longitudinal-with-death",
  "analysis/03-quality-of-life/05-longitudinal-unadjusted",
  "analysis/02-overall-survival/04-unrestricted-follow-up",
  "analysis/02-overall-survival/05-unrestricted-hierarchical",
  "analysis/02-overall-survival/06-censoring",
  "analysis/02-overall-survival/07-acceptance-kaplan-meier",
  "analysis/02-overall-survival/08-survival-rates",
  "analysis/02-overall-survival/09-unadjusted",

  "analysis/04-taooh/01-preparation.qmd",
  "analysis/04-taooh/02-imputation.qmd",
  "analysis/04-taooh/03-second-order-markov",
  "analysis/04-taooh/04-first-order-markov",
  "analysis/04-taooh/05-unadjusted-second-order",
  "analysis/05-progression-free-survival",
  "analysis/06-best-supportive-care",
  "analysis/07-blood-products",
  "analysis/08-tissue-biopsy",
  "analysis/09-imaging",
  "analysis/10-implementation"
)

notebooks <- unlist(lapply(run_order, function(entry) {
  if (grepl("\\.qmd$", entry)) {
    return(entry)
  }
  files <- list.files(here(entry), pattern = "\\.qmd$", recursive = TRUE)
  file.path(entry, sort(files, method = "radix"))
}))

stopifnot(all(file.exists(here(notebooks))), !anyDuplicated(notebooks))

args <- commandArgs(trailingOnly = TRUE)
option_value <- function(name) {
  position <- match(name, args)
  if (is.na(position)) NULL else args[[position + 1L]]
}

from <- option_value("--from")
if (!is.null(from)) {
  first <- which(startsWith(notebooks, from))[1]
  if (is.na(first)) stop("No notebook matches --from ", from)
  notebooks <- notebooks[first:length(notebooks)]
}

only <- option_value("--only")
if (!is.null(only)) {
  notebooks <- notebooks[startsWith(notebooks, only)]
  if (length(notebooks) == 0L) stop("No notebook matches --only ", only)
}

# Quarto renders with this R and the Rtools45 compiler path. Sorting uses the
# C collation, so factor levels and orderings do not depend on the machine;
# characters use UTF-8 so that labels with dashes or Greek letters print
# correctly (LC_ALL = C would turn them into "<U+2013>").
Sys.unsetenv("LC_ALL")
Sys.setenv(
  QUARTO_R = R.home("bin"),
  LC_COLLATE = "C",
  LC_CTYPE = "English_United States.utf8",
  MAKEFLAGS = "PATH=/x86_64-w64-mingw32.static.posix/bin:/usr/bin"
)

# Quarto scans the whole project directory before rendering. If a file
# disappears during that scan (e.g. while the report is rendered at the same
# time), the render fails with "os error 2): stat"; such renders are retried.
render <- function(notebook) {
  log_file <- tempfile(fileext = ".log")
  status <- system2(
    "quarto",
    c("render", shQuote(here(notebook))),
    stdout = log_file,
    stderr = log_file
  )
  log <- readLines(log_file, warn = FALSE)
  writeLines(log)
  list(status = status, scan_race = any(grepl("os error 2): stat", log, fixed = TRUE)))
}

for (notebook in notebooks) {
  message(format(Sys.time(), "%H:%M"), "  ", notebook)

  result <- render(notebook)
  attempt <- 1L
  while (result$status != 0L && result$scan_race && attempt < 3L) {
    attempt <- attempt + 1L
    message("Retrying after a Quarto project-scan error: ", notebook)
    result <- render(notebook)
  }

  if (result$status != 0L) stop("Rendering failed: ", notebook)
}
