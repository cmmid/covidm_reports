suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "simulate/CPV/001.qs", "simulate/CPV/peak.qs"
) else commandArgs(trailingOnly = TRUE)

aggregate.age <- function(dt) {
  setkeyv(rbind(
    dt[age != "all"],
    dt[age != "all", .(value = sum(value), age = "all") , by=setdiff(colnames(dt),c("age","value"))]
  ), key(dt))
}

aggregate.hosp <- function(dt) {
  setkeyv(rbind(
    dt[compartment != "hosp_p"],
    dt[compartment %in% c("nonicu_p","icu_p"),.(value = sum(value), compartment = "hosp_p") , by=setdiff(colnames(dt),c("compartment","value"))]
  ), key(dt))
}

aggregate.both <- function(dt) aggregate.hosp(aggregate.age(dt))

simfns <- sort(list.files(dirname(.args[1]), "\\d+\\.qs", full.names = TRUE))
ref <- aggregate.both(qread(simfns[1]))

refprobs <- c(lo.lo=0.025, lo=0.25, med=0.5, hi=0.75, hi.hi=0.975)

dys <- 0:(2*365)
milestones <- c(seq(30,90,by=30),seq(180,max(dys),by=90))

expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(unique(c(levels(ref$age), "all")), ordered = TRUE),
  t=dys
))

inc.expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(unique(c(levels(ref$age), "all")), ordered = TRUE),
  compartment=c("cases", "death_o"),
  t=dys
))

prev.expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  compartment=c("nonicu_p","icu_p","hosp_p"),
  t=dys
))

cprev.expander <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  compartment="R",
  t=dys
))

all.expand <- data.table(expand.grid(
  run=1:max(ref$run),
  age=factor(c(levels(ref$age), "all"), ordered = TRUE),
  compartment=c("cases","death_o", "R", "hosp_p","nonicu_p","icu_p"),
  t=dys
))

qdt <- function(dt) dt[,{
  qs <- quantile(value, probs = refprobs)
  names(qs) <- names(refprobs)
  as.list(qs)
}, by=.(compartment, t, age)]

#' assumes dt is already aggregated
full <- function(dt, scen_id) {
  inc <- dt[inc.expander, on=.(run, age, compartment, t)]
  inc[is.na(value), value := 0L ]
  prev <- dt[prev.expander, on=.(run, age, compartment, t)]
  prev[is.na(value), value := 0L ]
  cprev <- dt[cprev.expander, on=.(run, age, compartment, t), roll = TRUE]
  cprev[is.na(value), value := 0L ]
  qcprev <- qdt(cprev)
  qtmp <- qdt(rbind(inc, prev)[order(t),.(t, value = cumsum(value)), by=.(run, age, compartment)])
  rm(inc, prev)
  
  return(setkey(rbind(
    qtmp[order(t),.(
      t,
      lo.lo = diff(c(0,lo.lo)),
      lo = diff(c(0,lo)),
      med = diff(c(0,med)),
      hi = diff(c(0,hi)),
      hi.hi = diff(c(0,hi.hi))
    ), keyby=.(compartment, age)],
    qcprev
  ), compartment, age)[, scen_id := scen_id ])
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
  dt[compartment == comp,.SD[which.max(value)], by=.(run, compartment, age)][, measure := "peak" ]
}

cumul <- function(dt, comp = "cases") {
  ret <- dt[
    compartment == comp
  ][
    order(t),.(t, value = cumsum(value)), keyby=.(run, age)
  ][
    expander, on=.(run, age, t), roll = TRUE, rollends = c(F, T)
  ]
  ret[is.na(value), value := 0]
  setkey(ret, run, t, age)[, compartment := comp ][, measure := "acc" ]
}

exp_prevalence <- function(dt, comp) {
  ret <- dt[compartment == comp][
    expander[age != "all"], on=.(run, age, t),
    roll = F, rollends = c(F, F)
  ]
  ret[is.na(value), value := 0L]
  ret
}

calcAll <- function(dt) {
  rbind(
    peak_value(dt), # peak cases
    peak_value(dt, "death_o"), # peak deaths
    peak_value(dt, "R"), # peak infections
    cumul(dt), # cumulative cases
    cumul(dt, "death_o"), # cumulative deaths
    dt[compartment == "R"][, measure := "acc" ], # cumulative infections
    peak_value(dt, comp = "icu_p"),
    peak_value(dt, comp = "nonicu_p"),
    peak_value(dt, comp = "hosp_p"),
    cumul(exp_prevalence(dt,"icu_p"), "icu_p"),
    cumul(exp_prevalence(dt,"nonicu_p"), "nonicu_p"),
    cumul(dt, comp = "hosp_p")
  )
}

refvalues <- calcAll(ref)

accref <- refvalues[measure == "acc"][all.expand, on=.(run, compartment, age, t), roll = T, rollends = c(F, T)]
accref[is.na(value), value := 0 ]
accref[is.na(measure), measure := "acc" ]

accs <- list(
  melt(
    accref[t %in% milestones][,
      .(
       value = value,
       run, compartment, age, measure, t
      ),
    ],
    id.vars = c("run","compartment", "age", "t"),
    measure.vars = c("value"),
    variable.name = "metric"
  )[, {
    qs <- quantile(value, probs = refprobs, na.rm = TRUE)
    names(qs) <- names(refprobs)
    as.list(qs)
  }, keyby=.(metric, compartment, age, t)][, scen_id := 1 ]
)

alls <- list(full(ref, 1))

peaks <- list(
  melt(refvalues[
    measure == "peak",
    .(
      value = value,
      timing = t,
      run, compartment, age, measure
    )
    ],
    id.vars = c("run","compartment", "age"),
    measure.vars = c("value","timing"),
    variable.name = "metric"
  )[, {
    qs <- quantile(value, probs = refprobs)
    names(qs) <- names(refprobs)
    as.list(qs)
  }, keyby=.(metric, compartment, age)][, scen_id := 1L ]
)

for (ind in seq_along(simfns[-1])) {
  scen <- simfns[ind+1]
  scen_id <- as.integer(gsub("^.+/(\\d+)\\.qs$","\\1", scen))
  raw.dt <- aggregate.both(qread(scen))
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
  ][t %in% milestones]
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
  
  peaks[[ind + 1]] <- qs_peak[, scen_id := scen_id ]
  accs[[ind + 1]] <- qs_acc[, scen_id := scen_id ]
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
