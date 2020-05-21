suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "../covidm", "ZWE", "001", 
  sprintf("~/Dropbox/covidm_reports/hpc_inputs"),
  "simulate/ZWE/001.qs"
) else commandArgs(trailingOnly = TRUE)

# load covidm
cm_path = .args[1]
cm_force_rebuild = F;
cm_build_verbose = F;
cm_force_shared = T
cm_version = 1

suppressPackageStartupMessages({
  source(paste0(cm_path, "/R/covidm.R"))
})

# identify country / scenario
country <- .args[2]
scenario_index <- as.integer(.args[3])
inputpth <- path.expand(.args[4])
detailinputs <- sprintf("%s/%s", inputpth, country)
tarfile <- tail(.args, 1)
unmitigatedname <- gsub("\\d+\\.qs$","unmit_timings.qs", tarfile)

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

## overrides previous assignment, if any
if (dir.exists(detailinputs)) {
  for (.fn in list.files(detailinputs, "\\.(rds|qs)", full.names = TRUE)) {
    .nm <- gsub(sprintf("^%s/(.+)\\.(rds|qs)$", detailinputs),"\\1",.fn)
    assign(.nm, readRDS(.fn))
  }
}

set.seed(1234)

yu <- covidm_fit_yu[sample(.N, run_options[,max(index)], replace = TRUE)]

#' sub
iv_data <- alt_scenarios[scen_id == scenario_index][order(trigger_type)]

if (scenario_index != 1 && iv_data[,any(trigger_type %in% c("incidence","prevalence"))]){
  # should already have incidence set
  unmitigated <- qread(unmitigatedname)[compartment == "cases"]
  
  # unmitigated <- unmitigated[, .(value=sum(value)), by=c("run", "t", "compartment")]
}

#' set up paramaters
if(iv_data[,any(population != -1)]){
  hirisk_prop_isolated <- iv_data[coverage != 1, coverage]
  params <- params_set[[2]]
  #assign population actually isolated in high-risk
  params$pop[[1]][["size"]] <- params$pop[[1]][["size"]] + (1-hirisk_prop_isolated)*params$pop[[2]][["size"]]
  params$pop[[2]][["size"]] <- params$pop[[2]][["size"]]*(hirisk_prop_isolated)
  # TODO: define travel matrix here by relative population weights
  p1tot <- sum(params$pop[[1]][["size"]])
  p2tot <- sum(params$pop[[2]][["size"]])
  ptot <- p1tot + p2tot
  travelref <- matrix(0, 2, 2)
  travelref[1,1] <- p1tot/ptot
  travelref[1,2] <- p2tot/ptot
  travelref[2,2] <- p2tot/ptot
  travelref[2,1] <- p1tot/ptot
  params$travel <- travelref
} else {
  params <- params_set[[1]]
}

params_back <- params

ulim <- as.integer(Sys.getenv("SIMRUNS"))
#' @examples 
#' ulim <- 10
if (!is.na(ulim)) {
  warning(sprintf("running reduced set: %i", ulim))
  run_options <- run_options[1:min(ulim, .N)]
  yu <- yu[1:min(ulim, .N)]
}

yref <- unname(as.matrix(yu[, .SD, .SDcols = grep("y_",colnames(yu))]))
uref <- unname(as.matrix(yu[, .SD, .SDcols = grep("u_",colnames(yu))]))

has_age_split <- iv_data[, any(!is.na(age_split))]

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

cm_calc_R0_extended <- function(
  params
){

  infected_states <- c("E","Ia","Ip","Is")
  infected_states_entry <- c(1,0,0,0)
  ages <- c(1:length(params$pop[[1]]$size))
  pops <- sapply(params$pop, "[[", "name")
  
  duration <- function(distributed_times, tstep=params$time_step){
    #calculates the mean of the distribution
    sum(distributed_times * seq(0, by=tstep, length.out = length(distributed_times)))
  }
  
  #reduced transmission matrix
  #transmission matrix T times inverse of auxiliary matrix E
  transmission_reduced <- matrix(
    0,
    sum(infected_states_entry)*length(ages)*length(pops),
    length(infected_states)*length(ages)*length(pops)
  )
  
  #reduced transition matrix
  #negative of inversed transition matrix Sigma times auxilliary matrix E
  transition_reduced <- matrix(
    0,
    length(infected_states)*length(ages)*length(pops),
    sum(infected_states_entry)*length(ages)*length(pops)
  )
  
  for(p1 in 1:length(params$pop)){
    cm = Reduce('+', mapply(function(c, m) c * m, params$pop[[p1]]$contact, params$pop[[p1]]$matrices, SIMPLIFY = F))
    for(a1 in 1:length(params$pop[[p1]]$size)){
      for(p2 in 1:length(params$pop)){
        for(a2 in 1:length(params$pop[[p2]]$size)){
          for(s in 1:length(infected_states)){
            sj <- ti <- (p1-1)*length(params$pop[[p1]]$size)+a1
            si <- tj <- (p2-1)*length(params$pop[[p2]]$size)*length(infected_states)+(a2-1)*length(infected_states)+s
            
            trates <- c(
              "E" = 0,
              "Ia" =  params$pop[[p1]]$u[a1] *
                ifelse(
                  params$pop[[p1]]$size[a1] != 0,
                  params$pop[[p2]]$size[a2]/params$pop[[p1]]$size[a1],
                  0
                ) *
                #adjust beta if population size is scaled down
                # as probability with which people are contacted will be scale down if there
                # are less people of that age
                ifelse(
                  !is.null(params$pop[[p2]]$size_original),
                  params$pop[[p2]]$size[a2]/params$pop[[p2]]$size_original[a2],
                  1
                ) *
                cm[a1,a2] * 
                params$pop[[p2]]$fIa[a2] *
                params$travel[p1,p2] *
                ifelse(p1==p2,1,params$pop[[p2]]$tau[a2]),
              "Ip" = params$pop[[p1]]$u[a1] *
                ifelse(
                  params$pop[[p1]]$size[a1] != 0,
                  params$pop[[p2]]$size[a2]/params$pop[[p1]]$size[a1],
                  0
                ) *
                #adjust beta if population size is scaled down
                ifelse(
                  !is.null(params$pop[[p2]]$size_original),
                  params$pop[[p2]]$size[a2]/params$pop[[p2]]$size_original[a2],
                  1
                ) *
                cm[a1,a2] * 
                params$pop[[p2]]$fIp[a2] *
                params$travel[p1,p2] *
                ifelse(p1==p2,1,params$pop[[p2]]$tau[a2]),
              "Is" = params$pop[[p1]]$u[a1] *
                ifelse(
                  params$pop[[p1]]$size[a1] != 0,
                  params$pop[[p2]]$size[a2]/params$pop[[p1]]$size[a1],
                  0
                ) *
                #adjust beta if population size is scaled down
                ifelse(
                  !is.null(params$pop[[p2]]$size_original),
                  params$pop[[p2]]$size[a2]/params$pop[[p2]]$size_original[a2],
                  1
                ) *
                cm[a1,a2] * 
                params$pop[[p2]]$fIs[a2] *
                params$travel[p1,p2] *
                ifelse(p1==p2,1,params$pop[[p2]]$tau[a2])
            )
            
            #only applicable within one age
            if(p1==p2 & a1==a2){
              srates <- c(
                "E" = duration(params$pop[[p1]]$dE),
                "Ia" = (1-params$pop[[p1]]$y[a1])*duration(params$pop[[p1]]$dIa),
                "Ip" = params$pop[[p1]]$y[a1]*duration(params$pop[[p1]]$dIp),
                "Is" = params$pop[[p1]]$y[a1]*duration(params$pop[[p1]]$dIs)
              )  
            } else ( srates <- rep(0, 4) )
            
            transmission_reduced[ti,tj] <- trates[s]
            transition_reduced[si,sj] <- srates[s]
          }
        }
      }
    }
  }
  k <- transmission_reduced %*% transition_reduced
  #if(is.complex(eigen(k)$values)){
  #  warning("Eigenvalue is complex")
  #}
  return(max(Re(eigen(k)$values)))
}

allbind <- data.table()

for(i in 1:nrow(run_options)){

  params <- params_back
  
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
  
  rm(refcm)
  
  if(has_age_split) {
    params <- cm_split_matrices_ex_in(params, iv_data[!is.na(age_split), age_split])
  }
  
  ys <- rep(yref[i, ], each = 2)
  us <- rep(uref[i, ], each = 2)
  
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
  
  if (iv_data[,.N]) {
    iv = cm_iv_build(params)
    
    # generic interventions
    for (j in 1:nrow(iv_data[population == -1])) {
      #pars <- as.list(iv_data[population == -1][j])
      
      with(as.list(iv_data[population == -1][j]), {
        if (!is.na(trigger_type)) {
          if (trigger_type == "incidence") {
            start_day <- unmitigated[run == i & incidence >= trigger_value][1, t]
            end_day <- start_day + end_day
          } else if (trigger_type == "day") {
            start_day <- ifelse(is.na(timing$int0day), trigger_value, timing$int0day)
            end_day <- start_day + end_day
          } else if (trigger_type == "stride") {
            end_day <- seq(end_day, params$time1, by=trigger_value)
            start_day <- seq(start_day, params$time1, by=trigger_value)[1:length(end_day)]
          } else {
            stop(sprintf("do not understand trigger type %s", trigger_type))
          }
        }
        contact <- c(home, work, school, other)
        if (has_age_split) {
          contact <- c(contact, contact)
          if (!is.na(age_split)) {
            contact[1:4] <- 0
          }
        }
        cm_iv_set(iv,
                  as.Date(params$date0) + start_day,
                  as.Date(params$date0) + ifelse(is.finite(end_day),end_day,1e3),
                  fIs = 1-self_iso,
                  contact = 1-contact # TODO: manage splits
        )
      })
    }
    
    # specialized
    if(iv_data[,any(population != -1)]) with(as.list(iv_data[population != -1]), {
      travelupdate <- travelref
      travelupdate[1,1] <- travelref[1,1] + travelref[1,2]*travel
      travelupdate[1,2] <- travelref[1,2]*(1-travel)
      travelupdate[2,1] <- travelref[2,1]*(1-travel)
      travelupdate[2,2] <- travelref[2,2] + travelref[2,1]*travel
      if (!is.na(trigger_type)) {
        if (trigger_type == "incidence") {
          start_day <- unmitigated[run == i & incidence >= trigger_value][1, t]
          end_day <- start_day + end_day
        } else if (trigger_type == "day") {
          start_day <- ifelse(is.na(timing$int0day), trigger_value, timing$int0day)
          end_day <- start_day + end_day
        } else if (trigger_type == "stride") {
          start_day <- seq(start_day, params$time1, by=trigger_value)
          end_day <- seq(end_day, params$time1, by=trigger_value)
        } else {
          stop(sprintf("do not understand trigger type %s", trigger_type))
        }
      }
      # TODO: currently assumes only thing done with pop 2 is relative to travel matrix
      cm_iv_set(iv,
                as.Date(params$date0) + start_day,
                as.Date(params$date0) + ifelse(is.finite(end_day),end_day,1e3),
                travel = travelupdate
      )
    })
    
    params = cm_iv_apply(params, iv)
    rm(iv)
    
  }
  #run the model
  sim <- cm_simulate(
    params, 1,
    model_seed = run_options[i, model_seed]
  )$dynamics

  result <- reduce_ages(
    sim[
      compartment %in% c("cases","death_o","icu_p","nonicu_p","infections")
    ]
  )[,
    .(value = sum(value)),
    keyby = .(run, t, age, compartment)
  ][value != 0][, run := i ]
  
  rm(params)
  rm(sim)
  
  allbind <- rbind(allbind, result)
  gc()
}

if (scenario_index == 1){
  tpop <- sum(params_set[[1]]$pop[[1]]$size)
  inc <- allbind[compartment == "cases",.(
    incidence = sum(value)/tpop
  ), keyby=.(run, t, compartment)]
  qsave(inc, unmitigatedname)
}

qsave(allbind, tarfile)
