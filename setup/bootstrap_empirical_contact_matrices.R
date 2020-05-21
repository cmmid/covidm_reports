#' default contact matrices
suppressPackageStartupMessages({
  require(data.table)
  require(socialmixr) 
})

.args <- if (interactive()) c(
  sprintf("data/empirical_contact_matrices/%s/%s.csv","KEN",c("contacts","participants")),
  "KEN", 500,
  "~/Dropbox/covidm_reports/interventions/inputs/KEN/contact_matrices.rds"
) else commandArgs(trailingOnly = TRUE)

#' generate a new set of bootstrapped matrices if available
emp_contacts <- fread(.args[1])
emp_participants <- fread(.args[2])
target <- .args[3]
runs <- as.numeric(.args[4])

result <- tail(.args, 1)

survey_data <- survey(emp_participants, emp_contacts)

extract_cm <- function(
  survey = survey_data, filter,
  estimated.contact.age = "sample",
  estimated.participant.age = "sample",
  n = runs, bootstrap = TRUE, symmetric = FALSE,
  age.limits = seq(0, by = 5, length.out = 16),
  ...
) {
  inargs <- c(as.list(environment()), list(...))
  ext <- do.call(contact_matrix, inargs)
  inargs$n <- 2
  #' replace any samples with missing values
  return(lapply(lapply(ext$matrices, "[[", 1), function(mat) {
    while(sum(is.na(mat)) > 0){
      mat <- do.call(contact_matrix, inargs)$matrices[[1]]$matrix
    }
    return(mat)
  }))
}

cm_home <- extract_cm(filter = list(location="house"))
cm_other <- extract_cm(fil = list(location="other"))

#' currently don't have stratification by school or work
cm <- mapply(
  function(home, work, school, other) list(home=home, work=work, school=school, other = other),
  home = cm_home,
  other = cm_other,
  MoreArgs = list(
    work = cm_other[[1]]*0,
    school = cm_other[[1]]*0
  ),
  SIMPLIFY = FALSE
)

saveRDS(cm, tail(.args, 1))

