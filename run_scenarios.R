suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "helper_functions.R",
  "../covidm", "caboverde", "002", 
  sprintf("~/Dropbox/covidm_reports/hpc_inputs"),
  "caboverde/002.qs"
) else commandArgs(trailingOnly = TRUE)

source(.args[1])
cm_path = .args[2]
cm_version = 1
country <- .args[3]
scenario_index <- as.integer(.args[4])
inputpth <- path.expand(.args[5])
detailinputs <- sprintf("%s/%s", inputpth, country)
tarfile <- tail(.args, 1)
unmitigatedname <- gsub("\\d+\\.qs$","unmit_timings.qs", tarfile)


cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T

suppressPackageStartupMessages({
  source(paste0(cm_path, "/R/covidm.R"))
})

.inputfns <- list.files(inputpth, "\\.(rds|qs)", full.names = TRUE, include.dirs = F)

binReader <- function(nm) {
  switch(
    gsub("^.+(rds|qs)$","\\1",nm),
    rds = readRDS(nm),
    qs = qread(nm)
  )
}

for (.fn in .inputfns) {
  .nm <- gsub(sprintf("^%s/(.+)\\.(rds|qs)$",inputpth),"\\1",.fn)
  assign(.nm, binReader(.fn))
}

if (dir.exists(detailinputs)) {
  for (.fn in list.files(detailinputs, "\\.(rds|qs)", full.names = TRUE)) {
    .nm <- gsub(sprintf("^%s/(.+)\\.(rds|qs)$", detailinputs),"\\1",.fn)
    ## overrides previous assignment, if any
    assign(.nm, readRDS(.fn))
  }
}

set.seed(1234)

yu <- covidm_fit_yu[sample(.N, run_options[,max(index)], replace = TRUE)]

scen <- scenarios_overview[index == scenario_index, scen]
s <- scenarios_overview[index == scenario_index, s]

#' TODO this shadows country?
attach(scenarios[[scen]][s,])

if (scen != 1){
  # should already have incidence set
  unmitigated <- qread(unmitigatedname)[compartment == "cases"]
  # unmitigated <- unmitigated[, .(value=sum(value)), by=c("run", "t", "compartment")]
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

ulim <- as.integer(Sys.getenv("SIMRUNS"))
if (!is.na(ulim)) {
  cat("running reduced set: ",ulim,"...\n")
  run_options <- run_options[1:min(ulim, .N)]
  yu <- yu[1:min(ulim, .N)]
}

for(i in 1:nrow(run_options)){
    
  params <- params_back
  
  if (gen_socdist | (hirisk_prop_isolated > 0)) {
    iv = cm_iv_build(params)
    
    #' general social distancing
    if(gen_socdist){
      
      if(is.list(gen_socdist_start)){
        gen_socdist_startdate <- sapply(
          1:length(gen_socdist_start[[1]]),
          function(x){
            if(gen_socdist_start[[1]][x] == "incidence"){
              threshold_time <- unmitigated[run == i & incidence >= gen_socdist_schedule_filter_on_threshold[[1]][x]][1,t]
              if(is.na(threshold_time)){ threshold_time <- 1e6 }
              return(as.character(as.Date(params$date0) + threshold_time))
            } else {
              return(0)
            }
          })
      } else {
        threshold_time <- unmitigated[run == i & incidence >= gen_socdist_schedule_filter_on_threshold][1,t]
        if(is.na(threshold_time)){ threshold_time <- 1e6 }
        gen_socdist_startdate <- as.Date(params$date0) + threshold_time
      }
      
      if(is.list(gen_socdist_stop)){
        gen_socdist_stopdate <- as.Date(gen_socdist_startdate) + sapply(
          1:length(gen_socdist_stop[[1]]),
          function(x){ return(gen_socdist_stop[[1]][x]*round(365/12)) }
        )
      } else {
        gen_socdist_stopdate <- as.Date(gen_socdist_startdate) + gen_socdist_stop*round(365/12) 
      }
      
      gen_socdist_startdate <- as.Date(gen_socdist_startdate)
      gen_socdist_stopdate <- as.Date(gen_socdist_stopdate)
      
      if(length(gen_socdist_startdate) > 1){
        for(d in 2:length(gen_socdist_startdate)){
          if(gen_socdist_startdate[d] <= gen_socdist_stopdate[d-1]){ 
            gen_socdist_startdate[d] <- gen_socdist_stopdate[d-1]+1
          }
        } 
      }
      
      cm_iv_general_socdist(iv, gen_socdist_startdate, gen_socdist_stopdate, gen_socdist_contact)  
    }
    
    #' shielding
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
        threshold_time <- unmitigated[run == i & incidence >= hirisk_shield_schedule_filter_on_threshold][1,t]
        if(is.na(threshold_time)){ threshold_time <- 1e6 }
        hirisk_shield_startdate <- as.Date(params$date0) + threshold_time
      }
      
      hirisk_shield_stopdate <- as.Date(hirisk_shield_startdate) + hirisk_shield_stop*round(365/12)
      
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
  
  ys <- rep(
    yu[i, as.numeric(.SD[1]), .SDcols = grep("y_",colnames(yu))],
    each = 2
  )
  us <- rep(
    yu[i, as.numeric(.SD[1]), .SDcols = grep("u_",colnames(yu))],
    each = 2
  )
  
  params$pop <- lapply(
    params$pop,
    function(x){
      x$y <- ys
      x$u <- us
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
  )$dynamics[compartment %in% c("cases","death_o","icu_p","nonicu_p","E")]
  
  result[, "run"] <- i
  
  results_cases[[length(results_cases) + 1]] <- result
}

allbind <- rbindlist(results_cases)

if (scen == 1){
  tpop <- sum(params_set[[1]]$pop[[1]]$size)
  inc <- allbind[compartment == "cases",.(
    incidence = sum(value)/tpop
  ), keyby=.(run, t, compartment)]
  qsave(inc, unmitigatedname)
}

reduce_ages <- function (dt) {
  fctr <- function(i, lvls = c("<14", "15-29", "30-44","45-59", "60+")) factor(
    lvls[i], levels = lvls, ordered = T
  )
  dt[between(as.integer(group), 1, 3), age := fctr(1) ]
  dt[between(as.integer(group), 4, 6), age := fctr(2) ]
  dt[between(as.integer(group), 7, 9), age := fctr(3) ]
  dt[between(as.integer(group), 10, 12), age := fctr(4) ]
  dt[as.integer(group) >= 13, age := fctr(5) ]
}

qsave(reduce_ages(allbind)[, .(value = sum(value)), keyby=.(run, t, age, compartment)][value != 0], tarfile)
