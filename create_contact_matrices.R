#' default contact matrices
suppressPackageStartupMessages({
  require(data.table)
  #' this is the UPDATED socialmixr from https://github.com/jarvisc1/socialmixr
  require(socialmixr) 
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/interventions/generation_data/data_contacts_missing.csv",
  "../covidm", "uganda", 500,
  "~/Dropbox/covidm_reports/interventions/inputs/uganda/contact_matrices.rds"
) else commandArgs(trailingOnly = TRUE)

lookup <- fread(.args[1])[, short := tolower(gsub("[^a-zA-Z]","",name))]
cm_path <- .args[2]
target <- .args[3]
runs <- as.numeric(.args[4])
result <- tail(.args, 1)

#' generate a new set of bootstrapped matrices if available
emp_contacts <- sprintf("./data/empirical_contact_matrices/%s/contacts.csv", target)
emp_participants <- sprintf("./data/empirical_contact_matrices/%s/participants.csv", target)

if (file.exists(result)) {
  warning(sprintf("Skipping %s; already exists.", target))
} else if (file.exists(emp_contacts)){
  survey_data <- survey(fread(emp_participants), fread(emp_contacts))
  cm_home <- contact_matrix(
    survey_data,
    filter = list(location = "house"), estimated.participant.age = "sample",
    estimated.contact.age = "sample", bootstrap = TRUE, n = runs, symmetric = FALSE,
    age.limits = seq(0, by = 5, length.out = 16)
  )
  cm_home <- lapply(cm_home$matrices, "[[", 1)
  #' replace any samples with missing values
  cm_home <- lapply(cm_home, function(mat){
    if (sum(is.na(mat)) > 0){
      mat_curr <- mat
      while(sum(is.na(mat_curr)) > 0){
        mat_curr <- contact_matrix(
          survey_data,
          filter = list(location = "house"), estimated.participant.age = "sample",
          estimated.contact.age = "sample", bootstrap = TRUE, n = 2, symmetric = FALSE,
          age.limits = seq(0, by = 5, length.out = 16)
        )$matrices[[1]]$matrix
      }
      mat <- mat_curr
    }
    return(mat)
  })
  cm_other <- contact_matrix(
    survey_data,
    filter = list(location = "other"), estimated.participant.age = "sample",
    estimated.contact.age = "sample", bootstrap = TRUE, n = runs, symmetric = FALSE,
    age.limits = seq(0, by = 5, length.out = 16)
  )
  cm_other <- lapply(cm_other$matrices, "[[", 1)
  #' replace any samples with missing values
  cm_other <- lapply(cm_other, function(mat){
    if (sum(is.na(mat)) > 0){
      mat_curr <- mat
      while(sum(is.na(mat_curr)) > 0){
        mat_curr <- contact_matrix(
          survey_data,
          filter = list(location = "other"), estimated.participant.age = "sample",
          estimated.contact.age = "sample", bootstrap = TRUE, n = 2, symmetric = FALSE,
          age.limits = seq(0, by = 5, length.out = 16)
        )$matrices[[1]]$matrix
      }
      mat <- mat_curr
    }
    return(mat)
  })
  #' currently don't have stratification by school or work
  cm <- list()
  for(i in 1:runs){
    cm[[i]] <- list(
      "home" = cm_home[[i]],
      "work" = cm_other[[1]]*0,
      "school" = cm_other[[1]]*0,
      "other" = cm_other[[i]]
    ) 
  }
  saveRDS(cm, target)
} else {
  refcontactmatrices <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))
  found <- lookup[short == target, ifelse(cm, name, cm_name)]
  if (length(found)) {
    saveRDS(refcontactmatrices[[found[1]]], tail(.args, 1))
  } else {
    warning(sprintf("No matches found for %s; alternative not available yet.", target))
  } 
}

