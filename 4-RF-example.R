# 1-getting-started.R

.args <- if (!interactive()) commandArgs(trailingOnly = TRUE) else c("..")

# covidm options
cm_path = .args[1]; ### CHANGE THIS to reflect the path to covidm.
cm_force_rebuild = F;
cm_build_verbose = F;
source(paste0(cm_path, "/R/covidm.R"))

# Build parameters for a country
params = cm_parameters_SEI3R("Spain");

# Set an observation process
# TODO explain this better and make relevant
params$processes = list(
    list(source = "S", type = "multinomial", names = c("test1", "test2"), report = c("ipo", "ipo"),
        prob = matrix(c(rep(0.1, 16), rep(0.9, 16)), nrow = 2, ncol = 16, byrow = T),
        delays = matrix(c(cm_delay_gamma(7, 7, 60, 0.25)$p, cm_delay_gamma(14, 14, 60, 0.25)$p), nrow = 2, byrow = T)
    )
)

# what the above does.
# this says to set up an observation process. Individuals enter the process when they *leave* compartment [source].
# So e.g. if you want to track symptomatics (e.g. to track burdens) you should set source = "Is".
# When individuals enter this process, they get split (that's the type = "multinomial" bit -- only thing implemented so far) into
# two subprocesses, test1 and test2. prob is a [number of subprocesses] x [number of age groups] matrix with age-specific probabilities of
# entering either the "test1" or "test2" subprocess. These are unnormalised weights actually, so you can't just leave one blank at the moment.
# report = "ipo" means report the incidence, prevalence, and "outcidence" of individuals in the subprocesses. Can just specify e.g. "p" or "ip" or whatever.
# delays is how long individuals stay in the subprocesses and is not split by age. So far I just have cm_delay_gamma (for a gamma distribution,
# with parameters mean, shape, max_t and t_step, which needs to be the same as the simulation's time step).
# You can chain these together by setting the source of another process to one of the names of the subprocesses.

# run the model
run = cm_simulate(params)

# RR mode
# take age groups
# baseline risk
# age-based RR of outcomes
# age-based prevalence of comorbidities
# comorbidities RRs

comorbidity_key <- function(..., cols = NULL, sep = ".") {
    if (is.null(cols)) cols <- names(list(...))
    ids <- 1L:length(cols)
    res_key <- list()
    title <- "none"
    looks <- integer(0)
    res_key[title] <- list(looks)
    
    for (i in 1:length(cols)) {
        title <- apply(combn(cols, i), 2, function(cl) paste(cl, collapse=sep))
        slices <- combn(ids, i)
        looks <- lapply(1:dim(slices)[2], function(ind) slices[,ind])
        res_key[title] <- looks
    }
    res_key
}

comorbid.dt <- data.table(
    ages = 1:10, key = "ages"
)

comorbid.dt[, prev.A := seq(.1, .9, length.out = .N) ]
comorbid.dt[, RR.A := 1.1 ]
comorbid.dt[, prev.B := seq(.9, .1, length.out = .N) ]
comorbid.dt[, RR.B := 1.3 ]


#' creates a 
#'
#' @param comorbid.dt `data.table` with key columns (e.g. `age`, `region`)
#'  and `prev.X`, `RR.X` for each co-morbidity `X`
#' @param sep `character` separator for new group names
#'  
#' @return `data.table` with the same keys as
#'  `comorbid.dt`, and `prev.X`, `RR.X` columns
#'  however, `prev.X` now corresponds to only `X`, and `prev.X.Y`
#'  is prevalence of `X` and `Y` and so on. Same pattern for `RR.X`, `RR.X.Y`.
cm_create_risk_groups <- function(
    comorbid.dt, sep="."
) {
    #' get the comorbidity information; check that prev / RR
    #' are compatible
    prevs <- grep("prev\\.", colnames(comorbid.dt), value = TRUE)
    RRs   <- grep("RR\\.", colnames(comorbid.dt), value = TRUE)
    stopifnot(length(prevs) == length(RRs))
    cms <- gsub("prev\\.","",prevs)
    stopifnot(sort(cms) == sort(gsub("RR\\.","",prevs)))
    
    # make the full combinations key
    ckey <- comorbidity_key(cols=cms, sep=sep)
    # key = name, values = prev / RR to include
    
    # name all the new categories
    prev.nms <- sprintf("prev%s%s", sep, names(ckey))
    RR.nms <- sprintf("RR%s%s", sep, names(ckey))
    
    ps <- lapply(ckey, function(usecols, dt) {
        if (length(usecols) == dim(dt)[2]) { # all of the conditions
            apply(dt, 1, prod)
        } else if (!length(usecols)) { # none of the conditions
            apply(1-dt, 1, prod)
        } else { # mixtures of conditions
            apply(dt[,usecols,with=F],1,prod)*apply(1-dt[,-usecols,with=F],1,prod)
        }
    }, dt = comorbid.dt[, prevs, with = F])
    
    names(ps) <- paste("prev", names(ps), sep = sep)
    
    rs <- lapply(ckey, function(usecols, dt) {
        if (length(usecols) == dim(dt)[2]) { # all of the conditions
            apply(dt, 1, prod)
        } else if (!length(usecols)) { # none of the conditions
            1
        } else { # mixtures of conditions
            apply(dt[,usecols,with=F],1,prod)
        }
    }, dt = comorbid.dt[, RRs, with = F])
    
    
    
    ret.dt <- copy(comorbid.dt)[,.SD,keyby=key(comorbid.dt)]
    
}

# show results
ggplot(run$dynamics[compartment == "cases"]) +
    geom_line(aes(t, value, colour = group, group = group)) +
    facet_wrap(~population)
