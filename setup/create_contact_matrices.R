#' default contact matrices
suppressPackageStartupMessages({
  require(data.table)
  #' this is the UPDATED socialmixr from https://github.com/jarvisc1/socialmixr
  require(socialmixr) 
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/generation/data_contacts_missing.csv",
  "../covidm", "BEN", 500,
  "~/Dropbox/covidm_reports/hpc_inputs/BEN/contact_matrices.rds"
) else commandArgs(trailingOnly = TRUE)
#' @example 
#' .args <- gsub("BEN","VEN",.args)

lookup <- fread(.args[1], strip.white = FALSE)
cm_path <- .args[2]
target <- .args[3]
runs <- as.numeric(.args[4])
result <- tail(.args, 1)

cm_name <- lookup[iso == target, cm_name]
refcontactmatrices <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))[[cm_name]]

if (length(refcontactmatrices) != 4) stop(sprintf("something went wrong for matrices for %s.", target))

saveRDS(refcontactmatrices, tail(.args, 1))