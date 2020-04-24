suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "caboverde/peak.qs"
) else commandArgs(trailingOnly = TRUE)

refprobs <- c(lo.lo=0.025, lo=0.25, med=0.5, hi=0.75, hi.hi=0.975)

simfns <- sort(list.files(dirname(.args[1]), "\\d+\\.qs", full.names = TRUE))

# readsimf <- function(fn) {
#   res <- qread(fn)[, w := (t-1) %/% 7 ][w < 52]
#   res[,{
#     FUN <- if (compartment %like% "_p") max else sum
#     .(value = FUN(value))
#   },by=.(run,w,age,compartment)]
#   res
# }

ref <- qread(simfns[1])

expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  t=1:365
))

inc.expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  compartment=c("cases","death_o", "E"),
  t=1:365
))

prev.expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  compartment=c("nonicu_p","icu_p"),
  t=1:365
))

all.expand <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  compartment=c("cases","death_o", "E","hosp_p","nonicu_p","icu_p"),
  t=1:365
))

full <- function(dt, scen_id) {
  inc <- dt[inc.expander, on=.(run, age, compartment, t)]
  inc[is.na(value), value := 0L]
  prev <- dt[prev.expander, on=.(run, age, compartment, t)]
  prev[is.na(value), value := 0L]
  tmp <- rbind(inc, prev)
  # make the all ages category
  tmp <- rbind(tmp, tmp[,.(value = sum(value), age = "all"),by=.(run, t, compartment)])
  # make the all hospitalization category
  tmp <- rbind(tmp, tmp[compartment %in% c("nonicu_p","icu_p"),.(value = sum(value), compartment = "hosp_p"),by=.(run, t, age)])
  rm(inc, prev)
  qtmp <- tmp[order(t),.(t, value = cumsum(value)), by=.(run, age, compartment)][,{
    qs <- quantile(value, probs = refprobs)
    names(qs) <- names(refprobs)
    as.list(qs)
  }, by=.(compartment, t, age)]
  
  return(qtmp[order(t),.(
    t,
    lo.lo = diff(c(0,lo.lo)),
    lo = diff(c(0,lo)),
    med = diff(c(0,med)),
    hi = diff(c(0,hi)),
    hi.hi = diff(c(0,hi.hi))
  ),keyby=.(compartment, age)][, scen_id := scen_id ])
}


#' questions:
#'  - what is the unmitigated time line?
#'  - how does an intervention suppress (i.e., reduce [& delay for peaks]):
#'   * peak incidence of cases
#'   * total cases 3/6/9/12 mos out
#'   * peak incidence of deaths
#'   * total deaths 3/6/9/12 mos out
#'   * peak demand for nonicu
#'   * peak demand for icu
#'   * total icu days 3/6/9/12 mos out
#'   * total nonicu days 3/6/9/12 mos out

peak_value <- function(dt, comp = "cases") {
  ret <- dt[compartment == comp,.SD[which.max(value)], by=.(run, compartment, age)]
  retadd <- dt[
    compartment == comp,.(value = sum(value)), by=.(run, t, compartment)
  ][,
    .SD[which.max(value)],
    by=.(run, compartment)
  ][, age := "all"]
  setkey(rbind(ret, retadd), run, age)[, measure := "peak" ]
}

cumul <- function(dt, comp = "cases") {
  tmp <- dt[compartment == comp][order(t),.(t, value = cumsum(value)), keyby=.(run, age)]
  retadd <- dt[compartment == comp][,.(value = sum(value)), keyby=.(run, t)][order(t), .(age="all", t, value=cumsum(value)), keyby=.(run)]
  ret <- rbind(tmp, retadd)[expander, on=.(run, age, t), roll = TRUE, rollends = c(F, F)]
  setkey(ret[!is.na(value)], run, t, age)[, compartment := comp ][, measure := "acc" ]
}

exp_prevalence <- function(dt, comp) {
  ret <- dt[compartment == comp][
    expander[age != "all"], on=.(run, age, t),
    roll = T, rollends = c(F, F)
    ]
  ret[!is.na(value)]
}

combine_hosp_p <- function(dt) {
  comb <- rbind(
    exp_prevalence(dt, "icu_p"),
    exp_prevalence(dt, "nonicu_p")
  )
  comb[, .(value = sum(value), compartment = "hosp_p"), keyby=.(run, t, age)]
}

calcAll <- function(dt) {
  hospdt <- combine_hosp_p(dt)
  rbind(
    peak_value(dt), # peak cases
    peak_value(dt, "death_o"), # peak deaths
    peak_value(dt, "E"), # peak infections
    cumul(dt), # cumulative cases
    cumul(dt, "death_o"), # cumulative deaths
    cumul(dt, "E"), # cumulative infections
    peak_value(dt, comp = "icu_p"),
    peak_value(dt, comp = "nonicu_p"),
    peak_value(hospdt, comp = "hosp_p"),
    cumul(exp_prevalence(dt,"icu_p"), "icu_p"),
    cumul(exp_prevalence(dt,"nonicu_p"), "nonicu_p"),
    cumul(hospdt, comp = "hosp_p")
  )
}

refvalues <- calcAll(ref)

accref <- refvalues[measure == "acc"][all.expand, on=.(run, compartment, age, t), roll = T, rollends = c(F, T)]
accref[is.na(value), value := 0 ]
accref[is.na(measure), measure := "acc" ]

peaks <- list()
accs <- list()
alls <- list(full(ref, 1))

for (ind in seq_along(simfns[-1])) {
  scen <- simfns[ind+1]
  scen_id <- as.integer(gsub("^.+/(\\d+)\\.qs$","\\1", scen))
  raw.dt <- qread(scen)
  scenres <- calcAll(raw.dt)
  compar_peak <- scenres[
    measure == "peak"
  ][
    refvalues[measure == "peak"], on=.(run, compartment, age, measure)
  ]
  compar_peak[is.na(t), c("t","value") := .(Inf, 0)]
  
  qs_peak <- melt(
    compar_peak[,
      .(
        value = value,
        timing = t,
        delay = t-i.t,
        reduction = i.value - value,
        effectiveness = ifelse(i.value == value, 0, (i.value - value)/i.value), run, compartment, age, measure)
    ],
    id.vars = c("run","compartment", "age"),
    measure.vars = c("value","timing","delay","reduction","effectiveness"),
    variable.name = "metric"
  )[, {
    qs <- quantile(value, probs = refprobs)
    names(qs) <- names(refprobs)
    as.list(qs)
  }, keyby=.(metric, compartment, age)]
  rm(compar_peak)
  
  compar_acc <- scenres[
    measure == "acc"
  ][
    accref, on=.(run, compartment, age, measure, t), roll = T, rollends = c(F, T)
  ][t %in% c(30, 60, 90, 180, 270, 360)]
  compar_acc[is.na(value), value := 0 ]

  qs_acc <- melt(
    compar_acc[,
      .(
        value = value,
        reduction = i.value - value,
        effectiveness = ifelse(i.value == value, 0, (i.value - value)/i.value),
        run, compartment, age, measure, t
      ),
    ],
    id.vars = c("run","compartment", "age", "t"),
    measure.vars = c("value","reduction","effectiveness"),
    variable.name = "metric"
  )[, {
    qs <- quantile(value, probs = refprobs)
    names(qs) <- names(refprobs)
    as.list(qs)
  }, keyby=.(metric, compartment, age, t)]
  rm(compar_acc)
  
  peaks[[ind]] <- qs_peak[, scen_id := scen_id ]
  accs[[ind]] <- qs_acc[, scen_id := scen_id ]
  alls[[ind + 1]] <- full(raw.dt, scen_id)
}

peak.dt <- setkey(rbindlist(peaks), scen_id, metric, compartment, age)
accs.dt <- setkey(rbindlist(accs), scen_id, metric, compartment, age, t)
alls.dt <- setkey(rbindlist(alls), scen_id, compartment, age, t)

qsave(peak.dt, tail(.args, 1))
qsave(accs.dt, gsub("peak", "accs", tail(.args, 1)))
qsave(alls.dt, gsub("peak", "alls", tail(.args, 1)))

#' @examples
#' require(ggplot2)
#' ggplot(thing1[age == "all"]) + aes(t, fill=age) + geom_histogram(position = "stack") + theme_minimal()
#' ggplot(deaths_ref) + aes(t, value, group = run) + facet_grid(. ~ age) + geom_line(alpha = 0.2) + theme_minimal()
#' ggplot(compar_peak) + aes(age, reduction) + facet_grid(compartment ~ .) + geom_point() + theme_minimal()
#' ggplot(compar_peak) + aes(age, delay) + facet_grid(compartment ~ .) + geom_point() + coord_flip() + theme_minimal()
#' ggplot(compar_acc[age == "all"]) + aes(t, reduction) + facet_grid(compartment ~ ., scale = "free_y") + geom_point() + theme_minimal()
