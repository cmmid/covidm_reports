#' default contact matrices
suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/interventions/generation_data/data_contacts_missing.csv",
  "../covidm", "uganda",
  "~/Dropbox/covidm_reports/interventions/inputs/uganda/contact_matrices.rds"
) else commandArgs(trailingOnly = TRUE)

lookup <- fread(.args[1])[, short := tolower(gsub("[^a-zA-Z]","",name))]
cm_path <- .args[2]
target <- .args[3]
result <- tail(.args, 1)

if (file.exists(result)) {
  warning(sprintf("Skipping %s; already exists.", target))
} else {
  refcontactmatrices <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))
  found <- lookup[short == target, ifelse(cm, name, cm_name)]
  if (length(found)) {
    saveRDS(refcontactmatrices[[found[1]]], tail(.args, 1))
  } else {
    warning(sprintf("No matches found for %s; alternative not available yet.", target))
  }
}

