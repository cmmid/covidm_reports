#' default simulation parameters
suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/interventions/generation_data/data_contacts_missing.csv",
  "../covidm", "zimbabwe",
  "~/Dropbox/covidm_reports/interventions/inputs/zimbabwe/params_set.rds"
) else commandArgs(trailingOnly = TRUE)

reference = fread(.args[1])
cm_path = .args[2]
target = .args[3]
outfile = tail(.args, 1)

cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T;
source(paste0(cm_path, "/R/covidm.R"))

country <- cm_populations[gsub(" ","",gsub("[^a-zA-Z]","",tolower(as.character(name))))==target, unique(as.character(name))]
matref <- reference[name == country, ifelse(cm, name, cm_name)]
if (!length(matref)) matref <- country

stopifnot(length(country)==1)

#source("./helper_functions.R")

#set up node-runs
seed_cases <- 50

#general population parameters
params_set <- list()

params1 <- cm_parameters_SEI3R(
  country, matref,
  deterministic=FALSE
)

popnorm <- function(x){
  #no contact matrix - will be overwritten by empirical ones
  x$matrices$home <- x$matrices$work <- x$matrices$school <- x$matrices$other <- x$matrices$other*0
  
  #age-specific probability of being symptomatic
  #x$y <- c(rep(0.056, 3), rep(0.49, 8), rep(0.74, 8))
  #new values proposed by nick
  x$y <- c(
    rep(0.2973718, 2), rep(0.2230287, 2), rep(0.4191036, 2),
    rep(0.4445867, 2), rep(0.5635720, 2), rep(0.8169443, 6)
  )
  
  #no cases in empty compartments
  x$dist_seed_ages <- as.numeric(!(x$size == 0))
  
  #seed cases
  x$seed_times <- rep(0, seed_cases)
  
  return(x)
}

params1$pop <- lapply(params1$pop, popnorm)
params1$time1 <- as.Date(params1$time1)

params_set[[1]] <- params1

params2 <- cm_parameters_SEI3R(
  rep(country, 2), rep(matref, 2),
  deterministic=FALSE
)

#no shielding in <60yo, full eligibility for shielding in 60+
prop_highrisk <- c(rep(0, 12), rep(1, 4))

#no seeding events in high risk population
params2$pop[[2]]$name <- paste0(params2$pop[[2]]$name, " - high risk")
params2$pop[[2]]$size <- (prop_highrisk) * params2$pop[[1]]$size

params2$pop[[1]]$name <- paste0(params2$pop[[1]]$name, " - low risk")
params2$pop[[1]]$size <- (1 - prop_highrisk) * params2$pop[[1]]$size

params2$pop <- lapply(params2$pop, popnorm)
params2$pop[[2]]$seed_times <- 1e6

#normal mixing between populations
params2$travel <- matrix(rep(1, 4), 2)

params2$time1 <- as.Date(params2$time1)

params_set[[2]] <- params2

saveRDS(params_set, outfile)

