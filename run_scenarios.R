suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "helper_functions.R",
  "../covidm", "uganda", "001", 
  sprintf("~/Dropbox/covidm_reports/interventions/%s",c(
    "inputs",
    "uganda/001.qs"
  ))
) else commandArgs(trailingOnly = TRUE)

source(.args[1])
cm_path = .args[2]
country <- .args[3]
scenario_index <- as.integer(.args[4])
inputpth <- path.expand(.args[5])
detailinputs <- sprintf("%s/%s", inputpth, country)
tarfile <- tail(.args, 1)
unmitigatedname <- gsub("\\d+\\.qs$","unmit_timings.qs", tarfile)


cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T

source(paste0(cm_path, "/R/covidm.R"))

.inputfns <- list.files(inputpth, "\\.rds", full.names = TRUE, include.dirs = F)

for (.fn in .inputfns) {
  .nm <- gsub(sprintf("^%s/(.+)\\.rds$",inputpth),"\\1",.fn)
  assign(.nm, readRDS(.fn))
}

if (dir.exists(detailinputs)) {
  for (.fn in list.files(detailinputs, "\\.rds", full.names = TRUE)) {
    .nm <- gsub(sprintf("^%s/(.+)\\.rds$", detailinputs),"\\1",.fn)
    ## overrides previous assignment, if any
    assign(.nm, readRDS(.fn))
  }
}

scen <- scenarios_overview[index == scenario_index, scen]
s <- scenarios_overview[index == scenario_index, s]

#' TODO this shadows country?
attach(scenarios[[scen]][s,])

if (scen != 1){
  # should already have incidence set
  unmitigated <- qread(unmitigatedname)[compartment == "cases"]
}

#' set up paramaters
if(hirisk_prop_isolated > 0){
  params <- params_set[[2]]
  #assign population actually isolated in high-risk
  params$pop[[1]][["size"]] <- params$pop[[1]][["size"]] + (1-hirisk_prop_isolated)*params$pop[[2]][["size"]]
  params$pop[[2]][["size"]] <- params$pop[[2]][["size"]]*(hirisk_prop_isolated)
} else {
  params <- params_set[[1]]
}

results_cases <- list()

params_back <- params

for(i in 1:nrow(run_options)){
  
  params <- params_back
  
  if (gen_socdist | (hirisk_prop_isolated > 0)) {
    iv = cm_iv_build(params)
    #' general social distancing
    if(gen_socdist){
      if(gen_socdist_start == "incidence"){
        gen_socdist_startdate <- as.Date(params$date0) + unmitigated[run == i & incidence >= gen_socdist_schedule_filter_on_threshold][1,t]
      }
      gen_socdist_stopdate <- as.Date(params$date0) + gen_socdist_stop*30
      cm_iv_general_socdist(iv, gen_socdist_startdate, gen_socdist_stopdate, gen_socdist_contact)  
    }
    
    #' check if we need to adjust travel patterns
    if(hirisk_prop_isolated > 0){
      #assume same risk high-risk to low-risk as low-risk to high-risk
      update_travel <- matrix(rep(hirisk_lorisk_contact, 4), 2)
      #how much do high_risk groups contact each-other
      diag(update_travel) <- hirisk_contact
      #how much does low-risk group contact high-risk groups
      update_travel[1,] <- hirisk_lorisk_contact
      #no change in lorisk_lorisk contact
      update_travel[1,1] <- 1
      
      #shielding
      if(hirisk_shield_start == "incidence"){
        hirisk_shield_startdate <- as.Date(params$date0) + unmitigated[run == i & incidence > hirisk_shield_schedule_filter_on_threshold][1,t]
      }
      hirisk_shield_stopdate <- as.Date(params$date0) + hirisk_shield_stop*30
      
      iv[, travel := list(params$travel)]
      cm_iv_travel(iv, hirisk_shield_startdate, hirisk_shield_stopdate, list(update_travel))
    }
    
    params = cm_iv_apply(params, iv)
    
  }
  
  # if we have bootstrap 
  refcm <- if (is.null(names(contact_matrices))) { contact_matrices[[i]] } else { contact_matrices }
  names(refcm) <- gsub("cm_","",names(refcm))
  
  #use contact matrices for current sample
  params$pop <- lapply(
    params$pop,
    function(x){
      x$matrices <- refcm
      return(x)
    }
  )
  #adjust r0 to that in current sample
  target_R0 <- run_options[i, r0]
  params$pop <- lapply(
    params$pop,
    function(x){
      x$u <- x$u * (target_R0 / cm_calc_R0_extended(params))
      return(x)
    }
  )
  
  #' set up interventions
  params$pop <- lapply(
    params$pop,
    function(x){x$fIs <- x$fIs*symptomatic_contact; return(x)}
  )
  
  #run the model
  result <- cm_simulate(
    params,
    1,
    model_seed = run_options[i, model_seed]
  )$dynamics[compartment == "cases"]
  
  result[, "run"] <- i
  
  results_cases[[length(results_cases) + 1]] <- result
}

allbind <- rbindlist(results_cases)

if (scen == 1){
  tpop <- sum(params_set[[1]]$pop[[1]]$size)
  inc <- allbind[,.(
    incidence = sum(value)/tpop
  ), keyby=.(run, t, compartment)]
  qsave(inc, unmitigatedname)
}

qsave(allbind[value != 0], tarfile)
