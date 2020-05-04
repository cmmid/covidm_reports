input_dir <- path.expand("~/Dropbox/covidm_reports/interventions/inputs")
helper_file <- path.expand("~/workspaces/covidm_reports/helper_functions.R")

library(data.table)
source(helper_file)
  
# **************************************
#           main scen arios
# **************************************
#unmitigated
scenario_1 <- set_scenario_combinations(
  combine_each = list()
)

#50% self-isolation and 20/50% social-distancing
scenario_2 <- set_scenario_combinations(
  combine_each=list(
    gen_socdist=T,
    gen_socdist_stop=c(12),
    gen_socdist_house=1,
    gen_socdist_school="other",
    gen_socdist_work="other",
    gen_socdist_other=c(0.5, 0.8),
    symptomatic_contact=0.5,
    gen_socdist_start="incidence",
    gen_socdist_schedule_filter_on_threshold=c(1/10000)
  ),
  individual_only=list()
)

# #50% self-isolation and 50% social distancing
# scenario_3 <- set_scenario_combinations(
#   combine_each=list(
#     gen_socdist=T,
#     gen_socdist_stop=c(12),
#     gen_socdist_house=1,
#     gen_socdist_school="other",
#     gen_socdist_work="other",
#     gen_socdist_other=c(0.5),
#     symptomatic_contact=0.5,
#     gen_socdist_start="incidence",
#     gen_socdist_schedule_filter_on_threshold=c(1/10000)
#   ),
#   individual_only=list()
# )

shielding_coverage <- (1:4)/5
shielding_eff <- (1:4)/5

#50% self-isolation and 80% shielding
scenario_4 <- set_scenario_combinations(
  combine_each=list(
    symptomatic_contact=0.5,
    hirisk_shield_start="incidence",
    hirisk_shield_stop=12,
    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
    hirisk_prop_isolated=shielding_coverage,
    hirisk_contact=1,
    hirisk_lorisk_contact=shielding_eff
  ),
  individual_only=list()
)

#50% self-isolation and 80% shielding and 20/50% social-distancing
scenario_5 <- set_scenario_combinations(
  combine_each=list(
    symptomatic_contact=0.5,
    hirisk_shield_start="incidence",
    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
    hirisk_prop_isolated=shielding_coverage,
    hirisk_contact=1,
    hirisk_lorisk_contact=shielding_eff,
    hirisk_shield_stop=12,
    gen_socdist=T,
    gen_socdist_stop=c(12),
    gen_socdist_house=1,
    gen_socdist_school="other",
    gen_socdist_work="other",
    gen_socdist_other=c(0.5, 0.8),
    gen_socdist_start="incidence",
    gen_socdist_schedule_filter_on_threshold=c(1/10000)
  ),
  individual_only=list()
)

## **************************************
##          sensitivity analysis
##             2-month lockdown
## **************************************
#
##2 month lockdown and 50% self-isolation and 20% social-distancing
#scenario_7 <- scenario_2
#scenario_7[, "gen_socdist_stop"] <- list(c(
#  2,
#  scenario_7[, gen_socdist_stop]
#))
#scenario_7[, "gen_socdist_start"] <- list(c(
#  "incidence",
#  scenario_7[, gen_socdist_start]
#))
#scenario_7[, "gen_socdist_schedule_filter_on_threshold"] <- list(c(
#  1e-4,
#  scenario_7[, gen_socdist_schedule_filter_on_threshold]
#))
#scenario_7[, "gen_socdist_contact"] <- list(list(
#  c(1,0.2,0.2,0.2),
#  scenario_7[, gen_socdist_contact][[1]]
#))
#
##2month lockdown and 50% self-isolation and 50% social-distancing
#scenario_8 <- scenario_3
#scenario_8[, "gen_socdist_stop"] <- list(c(
#  2,
#  scenario_8[, gen_socdist_stop]
#))
#scenario_8[, "gen_socdist_start"] <- list(c(
#  "incidence",
#  scenario_8[, gen_socdist_start]
#))
#scenario_8[, "gen_socdist_schedule_filter_on_threshold"] <- list(c(
#  1e-4,
#  scenario_8[, gen_socdist_schedule_filter_on_threshold]
#))
#scenario_8[, "gen_socdist_contact"] <- list(list(
#  c(1,0.2,0.2,0.2),
#  scenario_8[, gen_socdist_contact][[1]]
#))
#
##2month lockdown and 50% self-isolation and 80% shielding
#scenario_9 <- set_scenario_combinations(
#  combine_each=list(
#    symptomatic_contact=0.5,
#    hirisk_shield_start="incidence",
#    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
#    hirisk_shield_stop=12,
#    hirisk_prop_isolated=0.8,
#    hirisk_contact=1,
#    hirisk_lorisk_contact=0.2,
#    gen_socdist=T,
#    gen_socdist_stop=c(2),
#    gen_socdist_house=1,
#    gen_socdist_school="other",
#    gen_socdist_work="other",
#    gen_socdist_other=c(0.2),
#    gen_socdist_start="incidence",
#    gen_socdist_schedule_filter_on_threshold=c(1/10000)
#  ),
#  individual_only=list()
#)
#
##two month lockdown 50% self-isolation and 80% shielding and 20% social-distancing
#scenario_10 <- scenario_5
#scenario_10[, "gen_socdist_stop"] <- list(c(
#  2,
#  scenario_10[, gen_socdist_stop]
#))
#scenario_10[, "gen_socdist_start"] <- list(c(
#  "incidence",
#  scenario_10[, gen_socdist_start]
#))
#scenario_10[, "gen_socdist_schedule_filter_on_threshold"] <- list(c(
#  1e-4,
#  scenario_10[, gen_socdist_schedule_filter_on_threshold]
#))
#scenario_10[, "gen_socdist_contact"] <- list(list(
#  c(1,0.2,0.2,0.2),
#  scenario_10[, gen_socdist_contact][[1]]
#))
#
##2month lockdown and 50% self-isolation and 80% shielding and 50% social-distancing
#scenario_11 <- scenario_6
#scenario_11[, "gen_socdist_stop"] <- list(c(
#  2,
#  scenario_11[, gen_socdist_stop]
#))
#scenario_11[, "gen_socdist_start"] <- list(c(
#  "incidence",
#  scenario_11[, gen_socdist_start]
#))
#scenario_11[, "gen_socdist_schedule_filter_on_threshold"] <- list(c(
#  1e-4,
#  scenario_11[, gen_socdist_schedule_filter_on_threshold]
#))
#scenario_11[, "gen_socdist_contact"] <- list(list(
#  c(1,0.2,0.2,0.2),
#  scenario_11[, gen_socdist_contact][[1]]
#))
#
## **************************************
##          sensitivity analysis
##            no self-isolation
## **************************************
#
##0% self-isolation and 20% social-distancing
#scenario_12 <- set_scenario_combinations(
#  combine_each=list(
#    gen_socdist=T,
#    gen_socdist_stop=c(12),
#    gen_socdist_house=1,
#    gen_socdist_school="other",
#    gen_socdist_work="other",
#    gen_socdist_other=c(0.8),
#    symptomatic_contact=1,
#    gen_socdist_start="incidence",
#    gen_socdist_schedule_filter_on_threshold=c(1/10000)
#  ),
#  individual_only=list()
#)
#
##0% self-isolation and 50% social-distancing
#scenario_13 <- set_scenario_combinations(
#  combine_each=list(
#    gen_socdist=T,
#    gen_socdist_stop=c(12),
#    gen_socdist_house=1,
#    gen_socdist_school="other",
#    gen_socdist_work="other",
#    gen_socdist_other=c(0.5),
#    symptomatic_contact=1,
#    gen_socdist_start="incidence",
#    gen_socdist_schedule_filter_on_threshold=c(1/10000)
#  ),
#  individual_only=list()
#)
#
##0% self-isolation and 80% social-distancing
#scenario_14 <- set_scenario_combinations(
#  combine_each=list(
#    symptomatic_contact=1,
#    hirisk_shield_start="incidence",
#    hirisk_shield_stop=12,
#    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
#    hirisk_prop_isolated=0.8,
#    hirisk_contact=1,
#    hirisk_lorisk_contact=0.2
#  ),
#  individual_only=list()
#)
#
##0% self-isolation and 80% shielding and 20% social-distancing
#scenario_15 <- set_scenario_combinations(
#  combine_each=list(
#    symptomatic_contact=1,
#    hirisk_shield_start="incidence",
#    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
#    hirisk_prop_isolated=0.8,
#    hirisk_contact=1,
#    hirisk_lorisk_contact=0.2,
#    hirisk_shield_stop=12,
#    gen_socdist=T,
#    gen_socdist_stop=c(12),
#    gen_socdist_house=1,
#    gen_socdist_school="other",
#    gen_socdist_work="other",
#    gen_socdist_other=c(0.8),
#    gen_socdist_start="incidence",
#    gen_socdist_schedule_filter_on_threshold=c(1/10000)
#  ),
#  individual_only=list()
#)
#
##0% self-isolation and 80% shielding and 50% social-distancing
#scenario_16 <- set_scenario_combinations(
#  combine_each=list(
#    symptomatic_contact=1,
#    hirisk_shield_start="incidence",
#    hirisk_shield_schedule_filter_on_threshold=c(1/10000),
#    hirisk_prop_isolated=0.8,
#    hirisk_contact=1,
#    hirisk_lorisk_contact=0.2,
#    hirisk_shield_stop=12,
#    gen_socdist=T,
#    gen_socdist_stop=c(12),
#    gen_socdist_house=1,
#    gen_socdist_school="other",
#    gen_socdist_work="other",
#    gen_socdist_other=c(0.5),
#    gen_socdist_start="incidence",
#    gen_socdist_schedule_filter_on_threshold=c(1/10000)
#  ),
#  individual_only=list()
#)

scenarios <- list(
  scenario_1,
  scenario_2,
#  scenario_3,
  scenario_4,
  scenario_5#,
#  scenario_6,
  #scenario_7,
  #scenario_8,
  #scenario_9,
  #scenario_10,
  #scenario_11,
  #scenario_12,
  #scenario_13,
  #scenario_14,
  #scenario_15,
  #scenario_16
)

scenarios_overview <- rbindlist(
  lapply(
    1:length(scenarios),
    function(x,z){ data.table(scen=rep(x, z[x]), s=1:z[x]) },
    z=sapply(scenarios, nrow)
  )
)
scenarios_overview[, "index"] <- c(1:nrow(scenarios_overview))

if(!dir.exists("./inputs")){
  dir.create("./inputs")
}

saveRDS(scenarios_overview, sprintf("%s/scenarios_overview.rds", input_dir))
saveRDS(scenarios, sprintf("%s/scenarios.rds", input_dir))
