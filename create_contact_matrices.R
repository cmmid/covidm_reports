#' default contact matrices

.args <- if (interactive()) c(
  "../covidm", "caboverde", "PLACEHOLDER",
  "~/Dropbox/covidm_reports/interventions/inputs/caboverde/contact_matrices.rds"
) else commandArgs(trailingOnly = TRUE)

cm_path <- .args[1]
target <- .args[2]
lookup <- .args[3]

refcontactmatrices <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))
found <- grep(target, tolower(gsub(" ","",names(refcontactmatrices))))

if (length(found)) {
  saveRDS(refcontactmatrices[[found[1]]], tail(.args, 1))
} else {
  warning(sprintf("No matches found for %s; alternative not available yet.", target))
}

