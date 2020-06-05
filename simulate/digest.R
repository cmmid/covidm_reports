suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_hpc_output/IDP/001.qs",
  "utils/sharedconst.rda",
  "simulate/IDP/peak.qs"
) else commandArgs(trailingOnly = TRUE)
#' @example use this to test alternatives
#' .args <- gsub("CPV","IDP",.args)

simfns <- sort(list.files(dirname(.args[1]), "\\d+\\.qs", full.names = TRUE))
load(.args[2])

ref <- aggregate.both(qread(simfns[1])[compartment %in% c("cases","death_o","icu_p","nonicu_p","R")])

#' @examples 
#' require(ggplot2)
#' ggplot(ref[run %in% sample(500, 50)]) +
#'   aes(t, value, group = run) +
#'   facet_grid(compartment ~ age, scale = "free_y") +
#'   geom_line(alpha = 0.1) +
#'   theme_minimal()

dys <- 0:(2*365)
milestones <- c(seq(30,90,by=30),seq(180,max(dys),by=90))

expander <- data.grid(
  run = 1:max(ref$run),
  age = factor(unique(c("all", levels(ref$age))), unique(c("all", levels(ref$age))), ordered = TRUE),
  compartment = factor(unique(ref$compartment), unique(ref$compartment), ordered = TRUE),
  t = dys
)

full.reinflate <- function(dt) {
  setkeyv(rbind(
    reinflate(dt, expander[compartment != "R"]),
    reinflate(dt, expander[compartment == "R"], roll = TRUE)
  ), key(dt))
}

reinflatedref <- full.reinflate(ref)

readsim <- function(fn) full.reinflate(aggregate.both(
  qread(fn)[compartment %in% c("cases","death_o","icu_p","nonicu_p","R")]
))

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

#' both assume dt already reinflated
peaks.dt <- function(dt, meas.name = "peak") dt[order(t), .SD[which.max(value)], by=.(run, compartment, age)][, measure := meas.name ]
cumul.dt <- function(dt, meas.name = "acc") dt[order(t), .(t, value = cumsum(value)), by=.(run, compartment, age)][, measure := meas.name ]

calcAll <- function(dt) {
  rbind(
    peaks.dt(dt), # all peaks
    cumul.dt(dt[compartment != "R"]), # R is already cumulative cumulative cases
    dt[compartment == "R"][, measure := "acc" ] # cumulative infections
  )
}

refvalues <- calcAll(reinflatedref)

#' @examples 
#' require(ggplot2)
#' ggplot(refvalues[measure == "peak" & compartment != "R"]) +
#'   aes(t, value, group = interaction(run, age), color = age) +
#'   facet_grid(compartment ~ ., scale = "free_y") +
#'   geom_point() +
#'   theme_minimal()
#'   
#' ggplot(refvalues[measure == "acc" & age == "all"]) +
#'   aes(t, value, group = interaction(run, age), color = age) +
#'   facet_grid(compartment ~ ., scale = "free_y") +
#'   geom_line(alpha = 0.1) +
#'   theme_minimal()
#'   

accref <- refvalues[measure == "acc"]
peakref <- refvalues[measure == "peak"]

full <- function(dt, scen_id) {
  noncumulative <- dt[compartment != "R"]
  cumulative <- dt[compartment == "R"]
  
  #' for compartments that are already cumulative, can just directly quantile
  qcprev <- quantile.dt(cumulative)
  #' for non-cumulative ones, want quantile by final size
  qtmp <- quantile.dt(noncumulative[
    order(t),
    .(t, value = cumsum(value)),
    by=setdiff(colnames(noncumulative),c("t","value"))
    ])[order(t),.(
      t,
      lo.lo = diff(c(0,lo.lo)),
      lo = diff(c(0,lo)),
      med = diff(c(0,med)),
      hi = diff(c(0,hi)),
      hi.hi = diff(c(0,hi.hi))
    ), by=.(compartment, age)]
  
  return(setkeyv(rbind(
    qtmp,
    qcprev
  ), setdiff(colnames(dt), c("run", "value")))[, scen_id := scen_id ])
}

accs <- list(
  quantile.dt(melt(
    accref[t %in% milestones][,
      .(
       value = value,
       run, compartment, age, measure, t
      ),
    ],
    id.vars = c("run","compartment", "age", "t"),
    measure.vars = c("value"),
    variable.name = "metric"
  ))[, scen_id := 1 ]
)

alls <- list(full(reinflatedref, 1))

peaks <- list(
  quantile.dt(melt(refvalues[
    measure == "peak",
    .(
      value = as.numeric(value),
      timing = as.numeric(t),
      run, compartment, age, measure
    )
    ],
    id.vars = c("run","compartment", "age"),
    measure.vars = c("value","timing"),
    variable.name = "metric"
  ))[, scen_id := 1L ]
)

for (ind in seq_along(simfns[-1])) {
  scen <- simfns[ind+1]
  scen_id <- as.integer(gsub("^.+/(\\d+)\\.qs$","\\1", scen))
  raw.dt <- readsim(scen)
  scenres <- calcAll(raw.dt)
  compar_peak <- scenres[
    measure == "peak"
  ][
    peakref, on=.(run, compartment, age, measure)
  ]
  compar_peak[is.na(t), c("t","value") := .(Inf, 0)]
  
  qs_peak <- quantile.dt(melt(
    compar_peak[,
      .(
        value = as.numeric(value),
        timing = as.numeric(t),
        delay = as.numeric(t-i.t),
        reduction = as.numeric(i.value - value),
        effectiveness = as.numeric(ifelse(i.value == value, 0, (i.value - value)/i.value)),
        run, compartment, age, measure)
    ],
    id.vars = c("run","compartment", "age"),
    measure.vars = c("value","timing","delay","reduction","effectiveness"),
    variable.name = "metric"
  ))
  rm(compar_peak)
  
  compar_acc <- scenres[
    measure == "acc"
  ][
    accref, on=.(run, compartment, age, measure, t), roll = T, rollends = c(F, T)
  ][t %in% milestones]
  compar_acc[is.na(value), value := 0 ]

  qs_acc <- quantile.dt(melt(
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
  ))
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
