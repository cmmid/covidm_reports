suppressPackageStartupMessages({
  require(data.table)
})
#' Create a list of LMIC locations
#' 
#' this is meant to be run from command line.
#' don't hard code paths, provide them as arguments

.args <- if (interactive()) c(
  "..", "100", "LMICargs.txt", "LMIC.txt", "CaboVerde-res.rds"
#' input path,
#' number to run,
#' country reference file,
#' which line from reference file,
#' stem of output file name
) else commandArgs(trailingOnly = TRUE)

cm_path = .args[1];
cm_force_rebuild = F;
cm_build_verbose = F;
source(paste0(cm_path, "/R/covidm.R"))

nruns <- as.integer(.args[2])
tarreffile <- .args[3]
ctyref <- .args[4]
outtar <- tail(.args, 1)

locind <- which(fread(tarreffile, header = F) == outtar)
loc <- fread(ctyref, header = F, sep = ",")[locind]$V1

probs = fread(
  "Age,Prop_symptomatic,IFR,Prop_inf_hosp,Prop_inf_critical,Prop_critical_fatal,Prop_noncritical_fatal,Prop_symp_hospitalised,Prop_hospitalised_critical
10,0.66,8.59E-05,0.002361009,6.44E-05,0.5,0,0,0.3
20,0.66,0.000122561,0.003370421,9.19E-05,0.5,9.47E-04,0.007615301,0.3
30,0.66,0.000382331,0.010514103,0.000286748,0.5,0.001005803,0.008086654,0.3
40,0.66,0.000851765,0.023423527,0.000638823,0.5,0.001231579,0.009901895,0.3
50,0.66,0.001489873,0.0394717,0.001117404,0.5,0.002305449,0.018535807,0.3
60,0.66,0.006933589,0.098113786,0.005200192,0.5,0.006754596,0.054306954,0.3
70,0.66,0.022120421,0.224965092,0.016590316,0.5,0.018720727,0.150514645,0.3
80,0.66,0.059223786,0.362002579,0.04441784,0.5,0.041408882,0.332927412,0.3
100,0.66,0.087585558,0.437927788,0.065689168,0.5,0.076818182,0.617618182,0.3"
)

reformat = function(P) {
  # no info to re-weight these, so assume 70-74 is like 70-79, and 75+ is like 80+
  return (c(rep(P[1:7], each = 2),P[8:9]))
}

P.icu_symp     = reformat(probs[, Prop_symp_hospitalised * Prop_hospitalised_critical]);
P.nonicu_symp  = reformat(probs[, Prop_symp_hospitalised * (1 - Prop_hospitalised_critical)]);
P.death_icu    = reformat(probs[, Prop_critical_fatal]);
P.death_nonicu = reformat(probs[, Prop_noncritical_fatal]);

burden_processes = list(
  list(source = "Ip", type = "multinomial", names = c("to_icu", "to_nonicu", "null"), report = c("", "", ""),
       prob = matrix(c(P.icu_symp, P.nonicu_symp, 1 - P.icu_symp - P.nonicu_symp), nrow = 3, ncol = 16, byrow = T),
       delays = matrix(c(cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 3, byrow = T)),
  list(source = "to_icu", type = "multinomial", names = "icu", report = "p",
       prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(10, 10, 60, 0.25)$p, nrow = 1, byrow = T)),
  list(source = "to_nonicu", type = "multinomial", names = "nonicu", report = "p",
       prob = matrix(1, nrow = 1, ncol = 16, byrow = T),
       delays = matrix(cm_delay_gamma(8, 8, 60, 0.25)$p, nrow = 1, byrow = T)),
  list(source = "Ip", type = "multinomial", names = c("death", "null"), report = c("o", ""),
       prob = matrix(c(P.death_nonicu, 1 - P.death_nonicu), nrow = 2, ncol = 16, byrow = T),
       delays = matrix(c(cm_delay_gamma(22, 22, 60, 0.25)$p, cm_delay_skip(60, 0.25)$p), nrow = 2, byrow = T))
)

# build parameters for all of UK, down to the regional level (level 2).
params = cm_parameters_SEI3R(loc, loc, deterministic = F);
params$processes = burden_processes
params$pop[[1]]$seed_times = 0:9

run = cm_simulate(params, nruns)

saveRDS(run, outtar)