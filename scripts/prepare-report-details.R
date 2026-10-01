# Summarize saved runs for reporting; never fit models or rerun imputation.
here::i_am("scripts/prepare-report-details.R")
source(here::here("R", "report-reader.R"))
for (resource in c("references.bib", "chicago-author-date-16th-edition.csl")) {
  stopifnot(file.copy(here::here(resource), here::here("reports", resource), overwrite = TRUE))
}
stopifnot(file.copy(here::here("sap", "SAP-v1.1.pdf"),
                   here::here("reports", "SAP-v1.1.pdf"), overwrite = TRUE))
library(dplyr)
output_dir <- here::here("reports", "_data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
model_paths <- list.files(here::here("artifacts"), pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
model_paths <- model_paths[(grepl("/models/", model_paths) |
  grepl("/model-[0-9]+\\.rds$|/bsc-diagnosis/(cause_specific|composite)-[0-9]+\\.rds$", model_paths)) &
  !grepl("metadata|/archive/|diagnosis-random-intercepts-trial", model_paths)]
model_cache <- file.path(output_dir, "model-runs.csv")
cached_models <- if (file.exists(model_cache)) read.csv(model_cache) else data.frame()
run_rows <- lapply(model_paths, function(path) {
  directory <- if (basename(dirname(path)) == "models") dirname(dirname(path)) else dirname(path)
  key <- sub(".*/artifacts/", "", directory)
  subtype <- sub("\\.rds$", "", sub("-[0-9]+\\.rds$", "", basename(path)))
  if (subtype != "model") key <- paste(key, subtype, sep = "/")
  if (nrow(cached_models) && file.info(path)$mtime <= file.info(model_cache)$mtime) {
    old <- cached_models[cached_models$analysis == key & cached_models$file == basename(path), ]
    if (nrow(old) == 1L) return(old)
  }
  message("Reading saved model: ", key, "/", basename(path))
  fit <- readRDS(path)
  if (inherits(fit$stanfit, "stanfit")) {
    sim <- fit$stanfit@sim
    chains <- sim$chains
    iterations <- sim$iter
    retained <- (sim$iter - sim$warmup) / sim$thin
    warmup <- sim$warmup
  } else if (inherits(fit, "blrm")) {
    chains <- fit$chains
    iterations <- fit$iter
    retained <- nrow(fit$draws) / chains
    # The fitted object retains total iterations and sampled draws, but not
    # the resolved warmup argument. Do not infer warmup if thinning is possible.
    warmup <- NA_integer_
  } else return(NULL)
  data.frame(analysis = key, file = basename(path), chains = chains,
             iterations = iterations, retained = retained, warmup = warmup)
}) |> bind_rows()
write.csv(run_rows, file.path(output_dir, "model-runs.csv"), row.names = FALSE)

imputation_paths <- list.files(here::here("artifacts"), pattern = "imput.*\\.rds$", recursive = TRUE, full.names = TRUE)
imputation_paths <- imputation_paths[!grepl("/archive/|diagnosis-random-intercepts-trial", imputation_paths)]
imputation_cache <- file.path(output_dir, "imputation-runs.csv")
cached_imputations <- if (file.exists(imputation_cache)) read.csv(imputation_cache) else data.frame()
if ("maxit" %in% names(cached_imputations)) {
  cached_imputations$maxit <- as.character(cached_imputations$maxit)
}
imputation_rows <- lapply(imputation_paths, function(path) {
  source <- sub(".*/artifacts/", "", path)
  if (nrow(cached_imputations) && file.info(path)$mtime <= file.info(imputation_cache)$mtime) {
    old <- cached_imputations[cached_imputations$source == source, ]
    if (nrow(old) == 1L) return(old)
  }
  object <- readRDS(path)
  fits <- if (inherits(object, "mids")) list(object) else if (is.list(object)) Filter(function(x) inherits(x, "mids"), object) else list()
  if (!length(fits)) return(NULL)
  data.frame(source = source,
    completed = sum(vapply(fits, function(x) x$m, numeric(1))),
    maxit = paste(sort(unique(vapply(fits, function(x) x$iteration, numeric(1)))), collapse = ", "),
    logged_events = sum(vapply(fits, function(x) nrow(x$loggedEvents) %||% 0L, integer(1))))
}) |> bind_rows()
write.csv(imputation_rows, file.path(output_dir, "imputation-runs.csv"), row.names = FALSE)

# Diagnostic displays from the existing MICE objects.
library(mice)
for (endpoint in c("overall-survival", "taooh")) {
  path <- if (endpoint == "taooh") "imputations/imputation-fit.rds" else "imputations.rds"
  fit <- readRDS(here::here("artifacts", "primary", endpoint, path))
  variables <- names(fit$method)[nzchar(fit$method) & !startsWith(fit$method, "~")]
  grDevices::png(here::here("reports", "_figures", paste0(endpoint, "-mice-traces.png")), width=1800, height=1500, res=180)
  print(plot(fit, y = variables))
  grDevices::dev.off()
  grDevices::png(here::here("reports", "_figures", paste0(endpoint, "-mice-distributions.png")), width=1800, height=1500, res=180)
  print(lattice::densityplot(fit, stats::reformulate(variables)))
  grDevices::dev.off()
}
