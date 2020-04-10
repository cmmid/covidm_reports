library("ggh4x")
library("qs")

cm_path = "~/workspace/covidm/"; ### CHANGE THIS to reflect the path to covidm.
cm_force_rebuild = F;
cm_build_verbose = F;
source(paste0(cm_path, "/R/covidm.R"))
source("~/workspace/covid_lmic/interventions/process_severity.R")

output_dir <- "~/Dropbox/covidm_hpc_output/"
input_dir <- "~/Dropbox/covidm_reports/interventions/"

country <- c("Uganda")
time_limit <- 365*(9/12)

RR_hrisk_hospitalised <- 1
RR_hrisk_crtical_case <- 1

scenario_overview <- readRDS(sprintf("%s/inputs/scenarios_overview.rds", input_dir))
scenarios <- readRDS(sprintf("%s/inputs/scenarios.rds", input_dir))
params_set <- readRDS(sprintf("%s/inputs/%s/params_set.rds", input_dir, tolower(country)))

full_dynamics <- lapply(
  params_set,
  function(x){
    y <- cm_simulate(x,1)$dynamics[compartment == "cases"]
    y[, value := 0]
    return(y)
  }
)

unmitigated <- qread(sprintf("%s/%s/001.qs", output_dir, tolower(country)))
unmitigated <- unmitigated[t <= time_limit]
unmitigated[, "scen"] <- 1
unmitigated[, "s"] <- 1
#rbindlist(
#  lapply(
#    unique(unmitigated[, run]),
#    function(x){
#      y <- full_dynamics[[length(unique(unmitigated[, population]))]]
#      y[, run := x]
#      y <- merge(y, unmitigated[run==x], by=c("run", "t", "group", "compartment"))
#      y[, value := value.y]
#      y[is.na(value.y), value := value.x]
#      return(y[, -c("value.x","value.y")])
#    }
#  )
#)

#' need to account for proportion in high-risk when converting to severity
prop_assumed_highrisk <- prop_highrisk <- cm_high_risk_prevalence(country, T)[, highrisk]
#no shielding in <25yo
prop_assumed_highrisk[c(1:12)] <- 0
#full shielding for >60yo
prop_assumed_highrisk[c(13:16)] <- 1

phrisk_by_age <- data.table(
  group = attr(unmitigated$group, "levels"),
  age_low = seq(0, by=5, length.out = 16),
  age_high = c(seq(4, by=5, length.out = 15), 120),
  prop_highrisk = prop_highrisk,
  prop_assumed_highrisk = prop_assumed_highrisk
  #prop_hrisk_in_shielded = 1 - pmax(0, prop_assumed_highrisk*shielded_prop - prop_highrisk),
  #prop_hrisk_in_nonshielded = pmax(0, prop_highrisk - prop_assumed_highrisk*shielded_prop)
)

z <- scenarios[[1]]
z[, "scen"] <- 1

unmitigated_bytime <- unmitigated[, .(value=sum(value)), by=c("run","population","t","compartment","scen","s")]
unmitigated <- unmitigated[, .(value=sum(value)), by=c("run", "population", "group", "compartment","scen","s")]

unmitigated <- merge(unmitigated, z, by="scen")

unmitigated <- merge(
  unmitigated,
  phrisk_by_age,
  by="group"
)
unmitigated <- merge(
  unmitigated,
  data.table(
    group=unique(unmitigated$group),
    N=params_set[[1]]$pop[[1]]$size
  ),
  by="group"
)

#how many of cases are high-risk?
#unmitigated[, prop_hrisk := max(0, prop_highrisk - prop_assumed_highrisk*hirisk_prop_isolated), by=c("group", "population")]
#unmitigated[grepl("high", population, fixed=T), prop_hrisk := 1 - max(0, prop_assumed_highrisk*hirisk_prop_isolated - prop_highrisk), by=c("group", "population")]
#' assume high risk are not more likely to be isolated
#unmitigated[, prop_hrisk := prop_highrisk*(1-hirisk_prop_isolated), by=c("group", "population")]
unmitigated[, prop_hrisk := prop_highrisk, by=c("group", "population")]
#unmitigated[grepl("high", population, fixed=T), prop_hrisk := 1 - max(0, prop_assumed_highrisk*hirisk_prop_isolated - prop_highrisk), by=c("group", "population")]

unmitigated_severity <- processSeverity(unmitigated, T, RR_hrisk_hospitalised,RR_hrisk_crtical_case)

cases_files <- list.files(sprintf("%s/%s", output_dir, tolower(country)))
cases_files <- cases_files[cases_files != "unmit_timings.qs"]
cases_datasets <- lapply(
  cases_files,
  function(x){
    y <- qread(sprintf("%s/%s/%s", output_dir, tolower(country), x))
    y <- y[t <= time_limit]
    
    y[, "scenario_index"] <- as.numeric(unlist(regmatches(x, gregexpr('\\(?[0-9]+', x))))
    y[, "country"] <- country
    
    y_byage <- y[, .(value=sum(value)), by=c("scenario_index", "country", "run", "compartment", "population", "group")]
    y_bytime <- y[, .(value=sum(value)), by=c("scenario_index", "country", "run", "compartment", "population", "t")]
    return(
      list("y_byage" = y_byage, "y_bytime" = y_bytime)
    )
  }
)

cases_bytime <- rbindlist(
  lapply(
    cases_datasets,
    "[[",
    "y_bytime"
  )
)

cases_data <- rbindlist(
  lapply(
    cases_datasets,
    "[[",
    "y_byage"
  )
)

rm(cases_datasets)

cases_data <- merge(
  cases_data,
  scenario_overview,
  by.y="index",
  by.x="scenario_index"
)

cases_data <- lapply(
  2:length(scenarios),
  function(x){
    z <- scenarios[[x]]
    z[, "s"] <- c(1:nrow(z))
    return(merge(cases_data[scen == x], z, by="s"))
  }
)

cases_bytime <- merge(
  cases_bytime,
  scenario_overview,
  by.y="index",
  by.x="scenario_index"
)

cases_bytime <- lapply(
  2:length(scenarios),
  function(x){
    z <- scenarios[[x]]
    z[, "s"] <- c(1:nrow(z))
    return(merge(cases_bytime[scen == x], z, by="s"))
  }
)

plotdata_a <- unmitigated_bytime
plotdata_a[, "scenario_name"] <- "unmitigated"
plotdata_b <- cases_bytime[[1]][symptomatic_contact==0.4]
plotdata_b[, "scenario_name"] <- "Reduction symptomatic contacts 60%"
plotdata_c <- cases_bytime[[2]][
  gen_socdist_stop==9 & gen_socdist_other==0.6
]
plotdata_c[, "scenario_name"] <- "Social distancing -40%"
plotdata_d <- cases_bytime[[3]][
  hirisk_shield_stop==9 & hirisk_contact==1 & hirisk_prop_isolated==0.8 &
    hirisk_lorisk_contact == 0.4
]
plotdata_d[, "scenario_name"] <- "Shielding 80% - reduce contact 60%"
plotdata_e <- cases_bytime[[4]][
  hirisk_shield_stop==9 & hirisk_contact==1 & hirisk_prop_isolated==0.8 &
    hirisk_lorisk_contact == 0.4 & gen_socdist_stop==9 & gen_socdist_other==0.6
]
plotdata_e[, "scenario_name"] <- "Shielding 80% - reduce contact 60%\nSocial distancing -40%"

plotdata <- rbindlist(
  list(
    plotdata_a[, c("scenario_name","population","run","t","value")],
    #plotdata_b[, c("scenario_name","population","run","t","value")]
    #plotdata_c[, c("scenario_name","population","run","t","value")]
    #plotdata_d[, c("scenario_name","population","run","t","value")]
    plotdata_e[, c("scenario_name","population","run","t","value")]
  )
)

ggplot(
  data=plotdata
)+geom_line(
  aes(
    x=t,
    y=value,
    group=run
  ),
  alpha=0.1,
  colour="#777777"
)+geom_line(
  data=plotdata[, .(
    median=median(value)
  ), by=c("population", "scenario_name", "t")],
  aes(
    x=t,
    y=median
  ),
  colour="#000000",
  size=1
)+theme_bw(
)+facet_nested(
  .~scenario_name+population
)

#ggplot(cases_bytime[t <= 365 & scen == 3, .(min95=quantile(value, 0.025), min=quantile(value, 0.25), max=quantile(value, 0.75), median=quantile(value, 0.5), max95=quantile(value, 0.975)), by=c("s","t")], aes(x=t))+facet_wrap(s~.)+geom_ribbon(aes(ymin=min95,ymax=max95), colour="yellow", fill="yellow")+geom_ribbon(aes(ymin=min,ymax=max), colour="orange", fill="orange")+geom_line(aes(y=median), colour="red")+theme_bw()
#
#ggplot(
#  cases_bytime[t <= 365 & scen == 4 & s==1],
#  aes(x=t, y=value, group=run)
#)+geom_line(
#  colour="#777777",
#  alpha=0.1
#)+theme_bw(
#)+facet_wrap("s")

cases_data_severity <- lapply(
  cases_data,
  function(x){
    x <- merge(
      x,
      phrisk_by_age,
      by="group"
    )
    x <- merge(
      x,
      data.table(
        group=unique(x$group),
        N=params_set[[1]]$pop[[1]]$size
      ),
      by="group"
    )
    
    #how many of cases are high-risk?
    #assume irrespective of proportion isolated
    #x[, prop_hrisk := max(0, prop_highrisk - prop_assumed_highrisk*hirisk_prop_isolated), by=c("group", "population","scen","s","run")]
    #x[grepl("high", population, fixed=T), prop_hrisk := 1 - max(0, prop_assumed_highrisk*hirisk_prop_isolated - prop_highrisk), by=c("group", "population","scen","s","run")]
    x[, prop_hrisk := prop_highrisk, by=c("group", "population","scen","s","run")]
    
    return(processSeverity(x, T, RR_hrisk_hospitalised, RR_hrisk_crtical_case))
  }
)

#need to merge parameters again
cases_data_severity <- lapply(
  2:length(scenarios),
  function(x){
    z <- scenarios[[x]]
    z[, "s"] <- c(1:nrow(z))
    return(merge(cases_data_severity[[x-1]][scen == x], z, by="s"))
  }
)

#compare with unmitihated
cases_data_severity_compare <- lapply(
  cases_data_severity,
  function(x){
    y <- merge(x, unmitigated_severity[,-c("s","scen")], by=c("run", "outcome"))
    colnames(y)[colnames(y) == "value.y"] <- "unmitigated_value"
    colnames(y)[colnames(y) == "value.x"] <- "value"
    y[, diff := value/unmitigated_value]
    return(y)
  }
)

#cases_byage <- lapply(
#  2:length(scenarios),
#  function(x){
#    z <- scenarios[[x]]
#    z[, "s"] <- c(1:nrow(z))
#    return(merge(cases_byage[scen == x], z, by="s"))
#  }
#)

ggplot(
  cases_data_severity_compare[[1]][outcome == "cases_hospitalised", .(
    low95=round((1-quantile(diff, 0.025))*100,1),
    low50=round((1-quantile(diff, 0.25))*100,1),
    median=round((1-median(diff))*100,1),
    high50=round((1-quantile(diff, 0.725))*100,1),
    high95=round((1-quantile(diff, 0.975))*100,1)
  ), by=c("symptomatic_contact")],
  aes(x = (1-symptomatic_contact)*100, xend=(1-symptomatic_contact)*100)
)+geom_segment(
  aes(
    y=low95,
    yend=high95
  ),
  colour="darkorange"
)+geom_segment(
  aes(
    y=low50,
    yend=high50
  ),
  colour="darkblue"
)+geom_point(
  aes(y=median),
  colour="#FF0000"
)+scale_y_continuous(
  limits=c(0,NA) 
)+theme_bw(
)+labs(
  x="Reduction in contacts symptomatic cases (%)",
  y="Reduction in cases who require hospitalisation (%)"
)


ggplot(
  cases_data_severity_compare[[2]][outcome == "cases_hospitalised" & gen_socdist_stop==9, .(
    low95=round((1-quantile(diff, 0.025))*100,1),
    low50=round((1-quantile(diff, 0.25))*100,1),
    median=round((1-median(diff))*100,1),
    high50=round((1-quantile(diff, 0.725))*100,1),
    high95=round((1-quantile(diff, 0.975))*100,1)
  ), by=c("gen_socdist_other", "gen_socdist_stop")],
  aes(x = (1-gen_socdist_other)*100, xend=(1-gen_socdist_other)*100)
)+geom_segment(
  aes(
    y=low95,
    yend=high95
  ),
  colour="darkorange"
)+geom_segment(
  aes(
    y=low50,
    yend=high50
  ),
  colour="darkblue"
)+geom_point(
  aes(y=median),
  colour="#FF0000"
)+facet_nested(
  .~gen_socdist_stop,
  labeller = ggplot2::labeller(
    gen_socdist_stop =  function(string, before = "Number of months\nintervention active: ", after=""){
      paste0(
        before,
        #(1-as.numeric(string))*100,
        as.numeric(string),
        after
      )
    }
  )
)+scale_y_continuous(
  limits=c(0,NA) 
)+theme_bw(
)+labs(
  x="Reduction in contacts outside of the household (%)",
  y="Reduction in cases who require hospitalisation (%)"
)

ggplot(
  cases_data_severity_compare[[3]][outcome == "cases_hospitalised", .(
    low95=round((1-quantile(diff, 0.025))*100,1),
    low50=round((1-quantile(diff, 0.25))*100,1),
    median=round((1-median(diff))*100,1),
    high50=round((1-quantile(diff, 0.725))*100,1),
    high95=round((1-quantile(diff, 0.975))*100,1)
  ), by=c("hirisk_prop_isolated", "hirisk_lorisk_contact", "hirisk_contact")],
  #aes(x = 100*hirisk_prop_isolated, xend=100*hirisk_prop_isolated)
  aes(x = 100*(1-hirisk_lorisk_contact), xend=100*(1-hirisk_lorisk_contact))
)+geom_segment(
  aes(
    y=low95,
    yend=high95
  ),
  colour="darkorange"
)+geom_segment(
  aes(
    y=low50,
    yend=high50
  ),
  colour="darkblue"
)+geom_point(
  aes(y=median),
  colour="#FF0000"
)+facet_nested(
  #hirisk_lorisk_contact~hirisk_contact,
  #hirisk_prop_isolated~hirisk_contact,
  hirisk_contact~hirisk_prop_isolated,
  labeller = ggplot2::labeller(
    hirisk_lorisk_contact =  function(string, before = "Red. contacts between\nhigh- low-risk: ", after="%"){
      paste0(
        before,
        (1-as.numeric(string))*100,
        #as.numeric(string),
        after
      )
    },
    hirisk_contact =  function(string, before = "Contacts within shielded\nrelative to baseline: ", after="%"){
      paste0(
        before,
        #(1-as.numeric(string))*100,
        as.numeric(string)*100,
        after
      )
    },
    hirisk_prop_isolated =  function(string, before = "Eligible high-risk who\nare shielded: ", after="%"){
      paste0(
        before,
        #(1-as.numeric(string))*100,
        as.numeric(string)*100,
        after
      )
    }
  )
)+scale_y_continuous(
  #limits=c(0,NA) 
)+theme_bw(
)+labs(
  #x="Eligibile high-risk who are isolated (%)",
  x="Reduction in contact between shielded and non-shielded (%)",
  y="Reduction in cases who require hospitalisation (%)"
)+geom_hline(
  yintercept=0
)

ggplot(
  cases_data_severity_compare[[4]][outcome == "cases_critical", .(
    low95=round((1-quantile(diff, 0.025)),1)*100,
    low50=round((1-quantile(diff, 0.25)),1)*100,
    median=round((1-median(diff)),1)*100,
    high50=round((1-quantile(diff, 0.725)),1)*100,
    high95=round((1-quantile(diff, 0.975)),1)*100
  ), by=c("hirisk_prop_isolated", "hirisk_lorisk_contact", "hirisk_contact", "gen_socdist_other")],
  aes(x = 100*(1-hirisk_lorisk_contact), xend=100*(1-hirisk_lorisk_contact))
)+geom_segment(
  aes(
    y=low95,
    yend=high95
  ),
  colour="darkorange"
)+geom_segment(
  aes(
    y=low50,
    yend=high50
  ),
  colour="darkblue"
)+geom_point(
  aes(y=median),
  colour="#FF0000"
)+facet_nested(
  hirisk_contact~gen_socdist_other+hirisk_prop_isolated,
  labeller = ggplot2::labeller(
    hirisk_lorisk_contact =  function(string, before = "Red. contacts between\nhigh- low-risk: ", after="%"){
      paste0(
        before,
        (1-as.numeric(string))*100,
        #as.numeric(string),
        after
      )
    },
    hirisk_contact =  function(string, before = "Contacts within shielded\nrelative to baseline: ", after="%"){
      paste0(
        before,
        #(1-as.numeric(string))*100,
        as.numeric(string)*100,
        after
      )
    },
    hirisk_prop_isolated =  function(string, before = "Eligible high-risk who\nare shielded: ", after="%"){
      paste0(
        before,
        #(1-as.numeric(string))*100,
        as.numeric(string)*100,
        after
      )
    },
    gen_socdist_other =  function(string, before = "Red. contacts\noutside home: ", after="%"){
      paste0(
        before,
        (1-as.numeric(string))*100,
        #as.numeric(string)*100,
        after
      )
    }
  )
)+scale_y_continuous(
  #limits=c(0,NA) 
)+theme_bw(
)+geom_hline(
  yintercept=0
)+labs(
  x="Reduction in contact between shielded and non-shielded (%)",
  y="Reduction in hospitalised cases (%)"
)


