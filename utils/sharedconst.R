suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "utils/report_ref.rds", "utils/sharedconst.rda"
) else commandArgs(trailingOnly = TRUE)

refprobs <- c(lo.lo=0.025, lo=0.25, med=0.5, hi=0.75, hi.hi=0.975)
age.order <- c("all","<14","15-29","30-44","45-49","60+")
age.annotate <- function(l, fmt="%s\n%s") sprintf(fmt, l, ifelse(l=="all","ages","years"))

#' TODO
#'  - define milestones here?
#'  - define target compartments here?
milestones <- c(90, 180, 270, 360, 540, 720)

#' `data.table` an expand.grid
#' 
#' @param ... normal args to `expand.grid`, except `stringsAsFactors`
#' @param stringsAsFactors overrides the default `expand.grid` value (replaces `TRUE` with `FALSE`)
data.grid <- function(
  ..., stringsAsFactors = FALSE
) data.table(expand.grid(..., stringsAsFactors = stringsAsFactors))

# TODO set factor order for ages; 'all' first?

#' for covidm results table, create an 'all' age category
#' 
#' @param dt a `data.table` with `age` and `value` columns
#' @param ky the key for the resulting table, if aggregation performed
#' @param agg.name the entry in the `age` column for aggregated rows
#' 
#' @return a `data.table` with additional rows for all ages values
aggregate.age <- function(dt, ky = key(dt), agg.name = "all") {
  return(if (dt[age == agg.name, .N]) { # if there are >0 rows with age == agg.name
    warning(sprintf("dt already has an '%s' age; no action taken.", agg.name))
    dt
  } else { # otherwise (expected condition)
    setkeyv(rbind(
      dt,
      dt[, .(value = sum(value), age = agg.name), by=setdiff(colnames(dt),c("age","value"))]
    ), ky)
  })
}

#' for covidm results table, create an 'hosp_p' compartment
#' 
#' @param dt a `data.table` with `compartment` and `value` columns
#' @param ky the key for the resulting table, if aggregation performed
#' @param agg.name the entry in the `compartment` column for aggregated rows
#' @param hosp.compartments the `compartment` values to aggregate
#' 
#' @return a `data.table` with additional rows for aggregated hospitalization compartment values
aggregate.hosp <- function(dt, ky = key(dt), agg.name = "hosp_p", hosp.compartments = c("icu_p","nonicu_p")) {
  return(if (dt[compartment == agg.name, .N]) {
    warning(sprintf("dt already has an '%s' compartment; no action taken.", agg.name))
    dt
  } else { setkeyv(rbind(
    dt,
    dt[
      compartment %in% hosp.compartments,
      .(value = sum(value), compartment = agg.name),
      by=setdiff(colnames(dt), c("compartment", "value"))
    ]
  ), ky) })
}

aggregate.both <- function(dt) aggregate.hosp(aggregate.age(dt))

expander <- function(
  dt, run = 1:max(dt$run),
  age = factor(unique(c("all", levels(dt$age))), unique(c("all", levels(dt$age))), ordered = TRUE),
  t = 0:max(dt$t),
  compartment = unique(dt$compartment),
  ...
) {
  rm(dt)
  do.call(data.grid,c(as.list(environment()), list(...)))
}

reinflate <- function(dt, exp, roll = FALSE, na.val = 0L) return(
  dt[
    exp, on = colnames(exp), roll = roll
  ][,
    value := ifelse(is.na(value), na.val, value)
  ]
)

full.reinflate <- function(dt, exp.with = expander(dt)) {
  setkeyv(rbind(
    reinflate(dt[compartment != "R"], exp.with[compartment != "R"]),
    reinflate(dt[compartment == "R"], exp.with[compartment == "R"])
  ), key(dt))
}

quantile.dt <- function(dt, ps = refprobs) dt[,{
  qs <- quantile(value, probs = ps)
  names(qs) <- names(ps)
  as.list(qs)
}, by=setdiff(colnames(dt), c("run", "value"))]

meltquantiles <- function(dt, probs = refprobs) {
  ky <- setdiff(names(dt), names(probs))
  setkeyv(melt(
    dt, measure.vars = names(probs)
  ), c(ky, "variable"))
}



.allscens <- readRDS(.args[1])
scens <- .allscens$scen_id
int.factorize <- with(.allscens[c(1:3,9:11,8,4:7)], function(s) factor(s, scen_id, label, ordered = TRUE))
rm(.allscens)

save(list = ls(), file = tail(.args, 1))