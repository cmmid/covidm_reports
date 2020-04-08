# 1-getting-started.R

.args <- if (!interactive()) commandArgs(trailingOnly = TRUE) else c("..")

# covidm options
cm_path = .args[1]; ### CHANGE THIS to reflect the path to covidm.
cm_force_rebuild = T;
cm_build_verbose = T;
#cm_force_shared = T

source(paste0(cm_path, "/R/covidm.R"))

# build parameters for all of UK, down to the national level (level 1).
params = cm_parameters_SEI3R(cm_uk_locations("UK", 1), deterministic = T);

# # alternatively: build parameters for another country.
# params = cm_parameters_SEI3R("Italy");

# run the model
run = cm_simulate(params, 1)

# show results
ggplot(run$dynamics[compartment == "cases"]) +
    geom_line(aes(t, value, colour = group, group = group)) +
    facet_wrap(~population)

