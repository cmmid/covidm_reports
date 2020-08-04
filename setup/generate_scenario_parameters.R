suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/hpc_inputs/alt_scenarios.rds"
) else commandArgs(trailingOnly = TRUE)

outfile <- tail(.args, 1)
reffile <- gsub("/(\\w+)\\.rds", "/report_ref.rds", outfile)

#' want:
#'  - unmitigated
#'  - generic social distancing in school / work / other, levels 20-80%, by 20%
#'  - continuous low level social distancing (20%) + 60+ (20-40-60 home reduction; 60-70-80-90 work/other)
#'  - continuous low level social distancing (20%) + some 60+ (coverage 20-40-60-80) have some effectiveness (20-40-60-80) all source reduction
#'  - schools shutdown + continuous low level social distance (20%) + weekly alternating intense social distancing (40-50-60-70)
#'  - historical lockdown relaxing in 30-60-90 days from start

#' create a table for entries; all percents REDUCTIONS; all times simulation DAY (so 0, ..., N)
#'  scen_id (not necessarily unique), age category (all = 0, or number equal to 1-16), self_isolation,
#'  population (both = 0, hi = 1, low = 2), school, home, work, other, travel, start_day, end_day, trigger_type (NA or number), trigger_value
#' if trigger_type == "day" + start_day == NA; end_day then interpreted as delta from trigger date; trigger_value corresponds to default day
#' if trigger_type == "stride", then intervention repeats with trigger_value stride

dg <- function(...) data.table(expand.grid(..., stringsAsFactors = FALSE))

ref <- function(
  age_split = NA,
  self_iso = 0,
  population = -1, coverage = 1, # coverage generally only applies to multi-pop models
  school = 0, home = 0, work = 0, other = 0, # recall == these are reductions
  travel = 0,
  start_day = NA_integer_, end_day = Inf,
  trigger_type = NA_character_,
  trigger_value = NA_real_,
  ...
) do.call(dg, c(as.list(environment()), list(...)))

#' TODO: ignore? going to just iterate over rows where scen_id = 1, and if it's empty, nothing to do
unmitigated <- ref(scen_type = "unmitigated")[, scen_id := 1 ]

reportref <- data.table(scen_id=integer(), label=character())
append_scenarios <- function(dt, new_scen_ids, new_labels, scen_type) {
  newdt <- data.table(scen_id = new_scen_ids, label = new_labels)
  rbind(dt, newdt)
}

scen_counter <- 1
tagscenario <- function(dt, sc) { 
  dt[, scen_id := (1:.N) + sc ]
  return(dt[.N, scen_id])
}

# cat(sprintf("unmitigated: %i", 1))
reportref <- append_scenarios(reportref, 1L, "Unmitigated")

#' all remaining scenarios consider some level of self-isolation with symptoms
#' and potentially different school effects
self_iso_lvls <- c(0.25, 0.5, 0.75)
schoolsonoff <- c(on=0, off=1) # on = 0 reduction, off = 100% reduction

#'  - generic social distancing in work / other, levels 20-80%, by 10%
#' social distancing levels; use minimum for combination interventions
soc_dist_lvls <- (1:4)/5

soc_dist_only <- ref(
  self_iso = self_iso_lvls,
  school = schoolsonoff,
  dist = soc_dist_lvls, # work and other are matched, so will substitute after
  start_day = NA,
  trigger_type = "incidence",
  trigger_value = 1/10000,
  scen_type = "work & other distancing, schools on/off"
)[, c("other", "work") := .(dist, dist) ]
soc_dist_only[school == 0, school := dist ]
soc_dist_only$dist <- NULL

scen_counter <- tagscenario(soc_dist_only, scen_counter)

# cat(sprintf("generic 20%% distancing: %i", soc_dist_only[self_iso == 0.25 & school != 1.0 & other == 0.2, scen_id]))
# cat(sprintf("generic 60%% distancing: %i", soc_dist_only[self_iso == 0.25 & school != 1.0 & other == 0.6, scen_id]))
reportref <- append_scenarios(
  reportref,
  soc_dist_only[self_iso == 0.25 & school != 1.0 & other %in% c(0.2, 0.6), scen_id],
  sprintf("%i%% distancing",round(c(0.2,0.6)*100)),
  "generic distancing"
)

#' reference for elder shielding
sixtyplus <- 13

low_soc_all <- ref(
  self_iso = self_iso_lvls[1],
  dist = soc_dist_lvls[1],
  trigger_type = "incidence",
  trigger_value = 1/10000,
)[, c("school", "other", "work") := .(dist, dist, dist) ]
low_soc_all$dist <- NULL

#' because both generic and extra will apply as multiplicative reductions:
#' want target, such that (1-target)*(1-soc_dist_lvls[1]) = (1-(dist+soc_dist_lvls[1]))
#' => target = 1-(1-(dist+soc_dist_lvls[1]))/(1-soc_dist_lvls[1])

adjust_reduction <- function(add, have = soc_dist_lvls[1]) 1-(1-(add+have))/(1-have)

combo_int <- function(custom, generic = low_soc_all, scen_type) {
  cust_ids <- unique(custom$scen_id)
  return(rbind(
    custom,
    generic[rep(1:.N, each=length(cust_ids))][, scen_id := rep(cust_ids, length.out = .N) ]
  )[, scen_type := scen_type ])
}


#'  - continuous low level social distancing (20%) + 60+ (20-40-60 home reduction; 60-80-100 work/other)
elder_shielding <- ref(
  age_split = sixtyplus,
  school = schoolsonoff["off"],
  dist = (1:3)/5, # note: this is target *additional* reduction, which will be applied atop the generic 20% reduction
  start_day = NA,
  trigger_type = "incidence",
  trigger_value = 1/10000
)[, c("home", "work", "other") := .(
  dist,
  adjust_reduction(dist),
  adjust_reduction(dist)
)]
elder_shielding$dist <- NULL
scen_counter <- tagscenario(elder_shielding, scen_counter)

# cat(sprintf("40%% elder sheilding at home + low level all other measures: %i", elder_shielding[home == 0.4, scen_id]))
reportref <- append_scenarios(
  reportref,
  elder_shielding[home == 0.4, scen_id],
  sprintf("%i%% in-home\nelder shielding", round(0.4*100))
)

elder_shielding <- combo_int(
  elder_shielding,
  low_soc_all,
  "elder-focused reductions, low general distancing, no school"
)

#'  - continuous low level social distancing (20%) + self-isolation (25%) +
#'    some high-risk pop (coverage 20-40-60-80) have some effectiveness (20-40-60-80) reduction in contact with pop 1
sequester <- ref(
  population = 2,
  coverage = (1:4)/5,
  travel = (1:4)/5,
  start_day = NA,
  trigger_type = "incidence",
  trigger_value = 1/10000
)
# don't need to define age, as that's set by sequester population
scen_counter <- tagscenario(sequester, scen_counter)

# cat(sprintf("40C-80R greenzone + low level all other measures: %i", sequester[travel == 0.8 & coverage == 0.4, scen_id]))
# cat(sprintf("60C-60R greenzone + low level all other measures: %i", sequester[travel == 0.6 & coverage == 0.6, scen_id]))
# cat(sprintf("80C-80R greenzone + low level all other measures: %i", sequester[travel == 0.8 & coverage == 0.8, scen_id]))

reportref <- append_scenarios(
  reportref,
  c(
    sequester[travel == 0.8 & coverage == 0.4, scen_id],
    sequester[travel == 0.6 & coverage == 0.6, scen_id],
    sequester[travel == 0.8 & coverage == 0.8, scen_id]
  ),
  sprintf("%iC-%iR GZ,\nbasic PHSM", c(0.4,0.6,0.8)*100, c(0.8,0.6,0.8)*100)
)

sequester <- combo_int(
  sequester,
  low_soc_all,
  "elder sequestration, low general distancing, school continues"
)

#'  - schools shutdown + continuous low level social distance (20%) + weekly alternating extra social distancing (40-50-60-70) => that + 20%
alternating <- ref(
  school = schoolsonoff["off"],
  dist = (4:7)/10,
  start_day = 0, end_day = 6,
  trigger_type = "stride",
  trigger_value = 14
)[, c("work", "other") := .(
  adjust_reduction(dist),
  adjust_reduction(dist)
)]
alternating$dist <- NULL
scen_counter <- tagscenario(alternating, scen_counter)

# cat("no school + intermittent 40%% lockdown: %i", alternating[work == 0.5, scen_id])
reportref <- append_scenarios(
  reportref,
  alternating[between(work-0.5,-1e-6,1e-6), scen_id], # not == 0.6, due to combination
  sprintf("basic PHSM,\nschool closure,\nintermittent\n+%i%% lockdown", 0.4*100)
)

alternating <- combo_int(
  alternating,
  low_soc_all,
  "intermittent lockdowns & school closure"
)
#' assume generics in place from start of alternating, rather than incidence
alternating[trigger_type == "incidence", c("start_day","trigger_type","trigger_value") := .(0,NA_character_, 0)]


#'  - high lockdown relaxing to generic low social distancing / self-iso in 30-60-90 days from start
lockdown <- ref(
  school = schoolsonoff["off"],
  #' TODO fix this
  dist = c(.3,.5,.7),
  start_day = NA,
  end_day = c(30, 60, 90),
  trigger_type = "day",
  trigger_value = 0
)[, c("work", "other") := .(
  adjust_reduction(dist),
  adjust_reduction(dist)
)]
lockdown$dist <- NULL

scen_counter <- tagscenario(lockdown, scen_counter)

# cat("lockdown, 30-60-90: ", lockdown[order(end_day), scen_id])
reportref <- append_scenarios(
  reportref,
  lockdown[work == 0.375][order(end_day), scen_id],
  sprintf("%i day 50%% lockdown,\nthen basic PHSM", c(30,60,90))
)

lockdown <- combo_int(
  lockdown,
  low_soc_all,
  "temporary population-wide lockdown"
)
#' assume these in place from start of lockdown, rather than incidence
lockdown[trigger_type == "incidence", c("trigger_type","trigger_value") := .("day", 0)]

all_scenarios <- setkey(rbind(
  soc_dist_only,
  alternating,
  elder_shielding,
  sequester,
  lockdown
), scen_id)

saveRDS(reportref, reffile)
saveRDS(all_scenarios, outfile)
