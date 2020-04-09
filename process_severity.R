#' RR_ are relative risks for those in high-risk group (based on comorbidity) to be 
#'  hospitalised and to be a critical case (when hospitalised)
processSeverity <- function(cases_byage, aggregated=F, RR_hrisk_psymp_hosp=2, RR_hrisk_phosp_crit=2){
  #' data dependancies
  cov_pyr_dir <- "~/workspace/2020-cov-pyramid"
  severity_prop <- as.data.table(fread(sprintf("%s/data/severity_proportions.csv", cov_pyr_dir)))
  
  #' for CFR (not yet implemented)
  #source(sprintf("%s/R/distribution_functions.R", cov_pyr_dir))
  
  #' assign shielded population - if available
  cases_byage[, "population"] <- "nonshielded"
  cases_byage[grepl("high risk", population, fixed=T), "population"] <- "shielded"
  
  #how many cases in hrisk/lrisk group
  cases_byage[, cases_hrisk := value*prop_hrisk]
  cases_byage[, cases_lrisk := (value - cases_hrisk)]
  
  #combine populations
  cases_byage <- cases_byage[, .(
    cases_hrisk=sum(cases_hrisk),
    cases_lrisk=sum(cases_lrisk),
    N=sum(N)
    ), by=c("scen","s","run","compartment","group","age_low", "age_high", "prop_highrisk")
  ]
  
  #' for high-risk populations,shift distribution down by 10 years
  severity_prop <- severity_prop[c(2:nrow(severity_prop))]
  #cfr_by_age_using <- cfr_by_age[2:length(cfr_by_age)]
  severity_prop[, "age_from"] <- c(0,10,20,30,40,50,60,70)
  severity_prop[, "age_to"] <- c(9, 19, 29, 39, 49, 59, 69, 120)
  
  model_ages <- data.table(
    agegrp = as.character(unique(cases_byage$group)),
    age_from = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75),
    age_to = c(4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,120)
  )
  
  #assign severity age-groups to model ages
  for(i in 1:nrow(severity_prop)){
    model_ages[age_from >= severity_prop[i, age_from] & age_to <= severity_prop[i, age_to], "severity_agegroup"] <- i
  }
  cases_byage <- merge(cases_byage, model_ages[,c("severity_agegroup", "agegrp")], by.x="group", by.y="agegrp")
  
  #combine by severity_agegroup
  cases_byage <- cases_byage[, .(
    cases_hrisk = sum(cases_hrisk),
    cases_lrisk = sum(cases_lrisk),
    pop_hrisk = sum(prop_highrisk*N)/sum(N),
    N = sum(N),
    age_low = min(age_low),
    age_high = max(age_high)
  ), by=c("scen","s","run", "severity_agegroup")]
  
  cases_hrisk <- dcast(
    cases_byage,
    scen+s+run~severity_agegroup,
    value.var="cases_hrisk"
  )
  
  cases_lrisk <- dcast(
    cases_byage,
    scen+s+run~severity_agegroup,
    value.var="cases_lrisk"
  )
  
  #' adjust risk in low-high group based on comorbidities and comorbidities
  lrisk_psymp_hosp <- severity_prop[, Prop_symp_hospitalised]/(
    cases_byage[
      scen==unique(scen)[1] & s==unique(s)[1] & run==unique(run)[1],
      pop_hrisk
    ]*RR_hrisk_psymp_hosp +
    (1-cases_byage[
      scen==unique(scen)[1] & s==unique(s)[1] & run==unique(run)[1],
      pop_hrisk
    ])
  )
  hrisk_psymp_hosp <- lrisk_psymp_hosp*RR_hrisk_psymp_hosp
  
  lrisk_phosp_crit <- severity_prop[, Prop_hospitalised_critical]/(
    cases_byage[
      scen==unique(scen)[1] & s==unique(s)[1] & run==unique(run)[1],
      pop_hrisk
    ]*RR_hrisk_phosp_crit +
    (1-cases_byage[
      scen==unique(scen)[1] & s==unique(s)[1] & run==unique(run)[1],
      pop_hrisk
    ])
  )
  hrisk_phosp_crit <- lrisk_phosp_crit*RR_hrisk_phosp_crit
  
  model_output_byage_cases_hospitalised <- cbind(
    cases_byage[severity_agegroup==1, c("scen", "s", "run")],
    (t(
        t(cases_lrisk[,as.character(1:nrow(severity_prop)),with=F]) * lrisk_psymp_hosp
        +t(cases_hrisk[,as.character(1:nrow(severity_prop)),with=F]) * hrisk_psymp_hosp
    ))
  )
  model_output_byage_cases_hospitalised[, "outcome"] <- "cases_hospitalised"
  
  model_output_byage_cases_critical <- cbind(
    cases_byage[severity_agegroup==1, c("scen", "s", "run")],
    (t(
      t(cases_lrisk[,as.character(1:nrow(severity_prop)),with=F]) * lrisk_psymp_hosp * lrisk_phosp_crit
      +t(cases_hrisk[,as.character(1:nrow(severity_prop)),with=F]) * hrisk_psymp_hosp * hrisk_phosp_crit
    ))
  )
  model_output_byage_cases_critical[, "outcome"] <- "cases_critical"
  
  model_output_byage <- rbindlist(list(
    model_output_byage_cases_hospitalised,
    model_output_byage_cases_critical
  ))
  model_output_byage <- melt(
    model_output_byage,
    measure.vars=as.character(1:8),
    variable.name="agegrp", value.name="value"
  )
  model_output <- model_output_byage[,
    .(value=sum(value)),
    by=c("scen","s","run", "outcome")
  ]

  if(aggregated){
    return(model_output)
  } else {
    return(model_output_byage)
  }
}
