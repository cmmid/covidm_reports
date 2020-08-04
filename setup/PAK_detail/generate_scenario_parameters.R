suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/hpc_detailed/alt_scenarios.rds"
) else commandArgs(trailingOnly = TRUE)

outfile <- tail(.args, 1)
reffile <- gsub("/(\\w+)\\.rds", "/report_ref.rds", outfile)

# say that 11 March was the 50 active infections date
mar23 <- as.integer(as.Date("2020-03-23") - as.Date("2020-03-01"))

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
self_iso_lvls <- c(0.25)
schoolslowhi <- c(low=0.8, hi=1) # on = 0 reduction, off = 100% reduction

#'  - generic social distancing in work / other, levels 20-80%, by 10%
#' social distancing levels; use minimum for combination interventions

work_dist <- c(0.2, 0.4)
home_dist <- c(0.2, 0.4)
other_dist <- c(0, 0.2)

low_soc_all <- ref(
  self_iso = self_iso_lvls[1],
  home = home_dist,
  school = schoolslowhi,
  other = other_dist,
  work = work_dist,
  trigger_type = "day",
  trigger_value = mar23
)[, start_day := 0 ][, end_day := 90 ]
scen_counter <- tagscenario(low_soc_all, scen_counter)

# cat(sprintf("40%% elder sheilding at home + low level all other measures: %i", elder_shielding[home == 0.4, scen_id]))
reportref <- append_scenarios(
  reportref,
  low_soc_all[, scen_id],
  sprintf("generic options")
)

#'  - schools shutdown + continuous low level social distance (20%) + weekly alternating extra social distancing (40-50-60-70) => that + 20%
alternating <- ref(
  self_iso = self_iso_lvls[1],
  school = schoolslowhi["hi"],
  home = max(home_dist),
  work = max(work_dist),
  other = max(other_dist),
  start_day = mar23, end_day = mar23+30,
  trigger_type = "stride",
  trigger_value = 90
)
scen_counter <- tagscenario(alternating, scen_counter)

# cat("no school + intermittent 40%% lockdown: %i", alternating[work == 0.5, scen_id])
reportref <- append_scenarios(
  reportref,
  alternating[, scen_id],
  sprintf("alternating")
)

all_scenarios <- setkey(rbind(
  low_soc_all,
  alternating
), scen_id)

saveRDS(reportref, reffile)
saveRDS(all_scenarios, outfile)
