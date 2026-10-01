source(here::here("R", "report-reader.R"))
sap <- paste(readLines(here::here("sap", "SAP.qmd"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
chapters <- c(here::here("reports", "index.qmd"), list.files(here::here("reports", "sections"), "\\.qmd$", full.names = TRUE))
for (path in chapters) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  quotes <- sub("^> ", "", lines[startsWith(lines, "> ")])
  for (quote in quotes) {
    if (!grepl(quote, sap, fixed = TRUE)) stop("Quotation not found in SAP: ", basename(path), ": ", quote)
  }
}
paths <- c(tempfile(pattern = "summary", fileext = ".csv"), tempfile(pattern = "estimand-draws", fileext = ".csv"))
file.create(paths)
stopifnot(identical(report_table_artifacts(list(artifacts = paths)), paths[1]))
unlink(paths)
cat("SAP quotations match; posterior-draw tables excluded.\n")
