#' Set travel interventions
#' 
#' Travel (contact) between different populations
#'
#' @param iv existing interventions data.table, first generated using cm_iv_build
#' @param ymd_iv_first_day first day of travel interventions YYYY-MM-DD
#' @param ymd_iv_last_day final day of travel interventions YYYY-MM-DD
#' @param tf matrix with travel interventions (aa, ba, ab,bb)
#'
#' @return updates the iv data.table, doesn't return anything
#' @export
#'
#' @examples
cm_iv_travel <- function(iv, ymd_iv_first_day, ymd_iv_last_day, tf)
{
  # Check column is present
  cm_iv_checkset(iv, "travel");
  
  # Truncate dates of applying intervention to simulation time bounds
  # TODO check this logic
  t0 = max(iv[ 1, date], ymd(ymd_iv_first_day));
  t1 = min(iv[.N, date] + 1, ymd(ymd_iv_last_day));
  
  # Apply intervention
  iv[date >= t0 & date <= t1, travel := tf];
}

#' Start multiple social distancing interventions at different times
#' 
#' akin to cm_iv_school_breaks
#'
#' @param iv existing interventions data.table, first generated using cm_iv_build
#' @param ymd_break_start vector with first days of social distancing interventions YYYY-MM-DD
#' @param ymd_break_end vector with final days of social distancing interventions YYYY-MM-DD
#' @param sf list with vectors of updated contact proportions
#'
#' @return updates the iv data.table, doesn't return anything
#' @export
#'
#' @examples
cm_iv_general_socdist <- function(iv, ymd_break_start, ymd_break_end, sf = c(0,0,0,0))
{
  for (i in 1:length(ymd_break_start)) {
    cm_iv_contact(iv, ymd_break_start[i], ymd_break_end[i], sf[[i]]);
  }
}

#this will create the data.table with scenario options
# combine_each will be combined with each other, and every individual parameter
# individual only will each be done once, not permutation
set_scenario_combinations <- function(combine_each=list(),individual_only=list()){
  
  if(length(names(combine_each)) != length(combine_each) | length(names(individual_only)) != length(individual_only)){
    error("List(s) are not named")
  }
  
  #these parameters are required to be in data.table with scenario options
  parameters_defaults <- list(
    #target_R0 = list(default = 2.5,required = "all"),
    symptomatic_contact = list(default = 1,required = "all"),
    gen_socdist = list(default = FALSE, required = "all"),
    gen_socdist_house = list(default = 1, required = "gen_socdist"),
    gen_socdist_school = list(default = 1, required = "gen_socdist"),
    gen_socdist_work = list(default = 1, required = "gen_socdist"),
    gen_socdist_other = list(default = 1, required = "gen_socdist"),
    gen_socdist_start_date = list(default = Sys.Date(), required = "gen_socdist", exclude = "gen_socdist_schedule"),
    gen_socdist_stop_date = list(default = Sys.Date(), required = "gen_socdist", exclude = c("gen_socdist_schedule","gen_socdist_stop_months")),
    gen_socdist_schedule_filter_on = list(default = "cumulative_cases", required = FALSE),
    gen_socdist_schedule_filter_on_threshold = list(default = 1e5, required = FALSE),
    #gen_socdist_schedule_filter_on = list(default = "cumulative_cases", required = "gen_socdist_schedule"),
    #gen_socdist_schedule_filter_on_threshold = list(default = 1e5, required = "gen_socdist_schedule"),
    hirisk_prop_isolated = list(default = 0,required = "all"),
    hirisk_groups = list(default = 1, required = "hirisk"),
    hirisk_contact = list(default = 1, required = "hirisk"),
    hirisk_lorisk_contact = list(default = 1, required = "hirisk"),
    hirisk_shield_start_date = list(default = Sys.Date(), required = "hirisk", exclude = "hirisk_shield_schedule"),
    hirisk_shield_stop_date = list(default = Sys.Date(), required = "hirisk", exclude = c("hirisk_shield_schedule","hirisk_shield_stop_months")),
    hirisk_shield_schedule_filter_on = list(default = "cumulative_cases", required= FALSE),
    hirisk_shield_schedule_filter_on_threshold = list(default = 1e5, "required" = FALSE)
    #hirisk_shield_schedule_filter_on = list(default = "cumulative_cases", required = "hirisk_shield_schedule"),
    #hirisk_shield_schedule_filter_on_threshold = list(default = 1e5, required = "hirisk_shield_schedule")
  )
  
  if(length(individual_only)>0){
    scenario_options <- rbindlist(
      lapply(
        seq_along(individual_only),
        function(y, n, i){
          z <- combine_each
          z[[n[[i]]]] <- y[[i]]
          rbindlist(lapply(
            purrr::cross(z),
            as.data.table
          ))
        },
        y=individual_only,
        n=names(individual_only)
      ),
      fill=TRUE
    )
  } else if(length(combine_each)>0){
    scenario_options <- rbindlist(lapply(
      purrr::cross(combine_each),
      as.data.table
    ))
  } else {
    scenario_options <- as.data.table(
      lapply(parameters_defaults[sapply(parameters_defaults, "[[", "required") == "all"], "[[", "default")
    )
    #return early
    for(i in colnames(scenario_options)){
      message(paste0("Did not specify required variable ", i,". Adding default value."))
    }
    scenario_options <- as.data.table(scenario_options)
    return(scenario_options)
  }
  
  required_parameters <- names(parameters_defaults)[
    sapply(
      sapply(
        parameters_defaults,
        "[[",
        "required"
      ),
      function(x){
        x=="all" | sum(grepl(x,colnames(scenario_options),fixed=T))>0
      }
    )
  ]
  required_parameters <- required_parameters[
    !sapply(
      lapply(
        parameters_defaults[required_parameters],
        "[[",
        "exclude"
      ),
      function(x){
        if(is.null(x)){
          return(FALSE)
        } else {
          sum(sapply(x, function(y){grepl(y, colnames(scenario_options), T)}))>0
        }
      }
    )
  ]
  add_required <- required_parameters[!required_parameters %in% colnames(scenario_options)]
  if(length(add_required)>0){
    for(i in 1:length(add_required)){
      message(paste0("Did not specify required variable ", add_required[i],". Adding default value."))
      scenario_options[, add_required[i] := parameters_defaults[[add_required[i]]][["default"]]]
    }
  }
  
  #set NA values to default
  na_columns <- colnames(scenario_options)[unlist(lapply(scenario_options,function(x){sum(is.na(x))>1}))]
  if(length(na_columns)>0){
    for(i in 1:length(na_columns)){
      scenario_options[is.na(get(na_columns[i])), na_columns[i] := parameters_defaults[[na_columns[i]]][["default"]]]
    }
  }
  
  if( sum(scenario_options[,gen_socdist])>0 ){
    for(i in 1:nrow(scenario_options)){
      scenario_options[i, "gen_socdist_contact"] <- list(c(
        scenario_options[i, gen_socdist_house],
        scenario_options[i, gen_socdist_school],
        scenario_options[i, gen_socdist_work],
        scenario_options[i, gen_socdist_other]
      ))
    }
  }
  
  #data.table may not print if assigned in a function (as in lapply used here)
  # this overrides that behaviour
  scenario_options <- as.data.table(scenario_options)
  return(scenario_options)
}

createFilterSingle <- function(f, f_threshold, schedule_row){
  return(
    switch(
      f,
      "cumulative_cases" = paste0( "(aggregated[, .(cumcases = cumsum(cases), t)][t == time, cumcases] >= ", f_threshold, ")" ),
      "epi_peak" = paste0( "(aggregated[t == time, S]/aggregated[t==0, S+E+Ia+Ip+Is+R] >= ", f_threshold, ")" ),
      "incidence" = paste0( "(aggregated[t == time, cases]/aggregated[t == time, S+E+Ia+Ip+Is+R] >= ", f_threshold, ")" ),
      NULL
    )
  )
}

createFilter <- function(schedule_row){
  paste0(
    c(
      createFilterSingle(schedule_row$filter_on, schedule_row$filter_on_threshold, schedule_row),
        createFilterSingle(schedule_row$filter_off, schedule_row$filter_off_threshold, schedule_row)
    ),
    collapse=" & "
  )
}

assignSchedule <- function(parameter, change, filter_on, filter_on_threshold, filter_off=NULL, filter_off_threshold=NULL){
  get_threshold <- function(f, f_threshold, parameter, change){
    r0_argument_dynamic <- function(parameter, change){
      tmp_function <- function(x){
        eval(parse(text=paste0("cm_calc_R0_extended(params, ", parameter, " = x)")))
      }
      return(tmp_function(change))
    }
    return(
      switch(
        as.character(f_threshold),
        "initial" = 1/cm_calc_R0_extended(params),
        "intervention" = 1/r0_argument_dynamic(parameter, change),
        f_threshold
      )
    )
  }
  
  old_off_th <- filter_off_threshold
  new_off_th <- get_threshold(filter_off, filter_off_threshold, parameter, change)
  
  z <- data.table(
    filter_on=filter_on,
    filter_on_threshold=get_threshold(filter_on, filter_on_threshold, parameter, change),
    filter_off=filter_off,
    filter_off_threshold=get_threshold(filter_off, filter_off_threshold, parameter, change),
    parameter=parameter,
    change=ifelse(is.list(change),change,list(change))
  )

  if(exists("schedule_table", .GlobalEnv)){
    schedule_table <- get("schedule_table", .GlobalEnv)
    schedule_table <- rbindlist(list(schedule_table, z), fill=T)
  } else {
    schedule_table <- z
  }
  assign("schedule_table", schedule_table, envir=.GlobalEnv)
}

cm_iv_schedules_apply <- function(params){
  if(exists("schedule_table", .GlobalEnv)){
    schedule_table <- get("schedule_table", envir = .GlobalEnv)
    params$pop <- lapply(
      params$pop,
      function(x){
        x[["observer"]] <- function(time, dynamics){
          
          actions <- list();
          actions$changes <- list();
          
          schedule_table <- get("schedule_table", .GlobalEnv)
          
          aggregated <- dynamics[t <= time, lapply(.SD, sum, na.rm=TRUE), by="t", .SDcols=c("S","E","Ia","Ip","Is","R","cases") ]
          
          for(i in 1:nrow(schedule_table)){
            
            if( eval(parse(text=createFilter(schedule_table[i]))) ){
              list_action <- schedule_table[i, change][[1]]
              actions$changes[[schedule_table[i, parameter]]] <- list_action
              assign("intervention_triggered",TRUE, envir=.GlobalEnv)
            } else {
              if(schedule_table[i, parameter] %in% names(x)){
                actions$changes[[schedule_table[i, parameter]]] = x[[schedule_table[i, parameter]]]  
              } else {
                actions$changes[[schedule_table[i, parameter]]] = params[[schedule_table[i, parameter]]]
              }
              assign("intervention_triggered",FALSE, envir=.GlobalEnv)
            }
          }
          
          return(actions)
          
        }
        return(x)
      }
    )
  } else {
    message("No schedules registered")
  }
  return(params)
}

cm_calc_R0_extended <- function(
  params,
  ...,
  population_index = NULL,
  ngm=FALSE
){
  ellips <- list(...)
  
  if(!is.null(population_index)){
    params$pop <- params$pop[[population_index]]
  }
  
  if(length(ellips)>0){
    for(e in 1:length(ellips)){
      if(names(ellips)[e] %in% names(params)){
        params[[names(ellips)[e]]] <- ellips[[e]]
      } else if(names(ellips)[e] %in% names(params$pop[[1]])){
        params$pop <- lapply(
          params$pop,
          function(x, y){
            x[[names(y)]] <- y[[1]]
            return(x)
          },
          y=ellips[e]
        )
      } else {
        warning(paste0("Not sure what to do update with ",names(ellips)[[e]]))
      }
    } 
  }
  
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
  r0 <- max(Re(eigen(k)$values))
  if(ngm==TRUE){
    return(list("NGM"=k, "R0"=r0))
  } else {
    return(r0)
  }
}
