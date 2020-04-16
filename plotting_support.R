suppressPackageStartupMessages({
  require(qs)
  require(data.table)
  require(ggplot2)
})

.args <- if (interactive()) c(
  "caboverde/001.qs", "caboverde/alls.qs"
) else commandArgs(trailingOnly = TRUE)

refprobs <- c(lo.lo=0.025, lo=0.25, med=0.5, hi=0.75, hi.hi=0.975)

unmitigated.addall <- function(dt) setkeyv(rbind(
  dt,
  dt[, .(value = sum(value), age ="all"), by=setdiff(key(dt),"age")]
), key(dt))

unmitigated.peaks <- function(dt) dt[,
  .SD[which.max(value)]
  ,keyby=setdiff(key(dt), "t")
]

ggplotruns <- function(
  dt,
  override.aes = aes(),
  fade = 2^(-log10(dt[,max(run)])-1)
) ggplot(dt) +
  aes(t, value, group = run) +
  override.aes +
  geom_line(alpha = fade)

ggplotqs <- function(
  dt,
  override.aes = aes(),
  gm = geom_line,
  ...
) ggplot(dt) +
  aes(t, value) + override.aes +
  gm(...)


meltquantiles <- function(dt, probs = refprobs) {
  ky <- setdiff(names(dt), names(probs))
  setkeyv(melt(
    dt, measure.vars = names(probs)
  ), c(ky, "variable"))
}

scale_x_t <- function(
  name="Days since initial infection seeding",
  ...
) scale_x_continuous(name, ...)

scale_y_cases <- function(
  name="Incidence",
  ...
) scale_y_continuous(name, ...)

scale_y_hosp <- function(
  name="Current Hospitalisations",
  ...
) scale_y_continuous(name, ...)

peak_target_filter <- function(
  dt,
  ps = c(lo.lo=0.025, lo=0.25, med=0.5, hi=0.75, hi.hi=0.975)
) {
  unmitigated.peaks(dt)[,{
    pqs <- quantile(value, ps)
    names(pqs) <- sprintf("p.%s",names(ps))
    tqs <- quantile(t, ps)
    names(tqs) <- sprintf("t.%s",names(ps))
    as.list(c(pqs,tqs))
  }, keyby=setdiff(key(dt),c("t","run"))]
}

geom_target <- function(
  aes.ref,
  ...
) {
  stopifnot(c("xmid","ymid") %in% names(aes.ref))
  aeslr <- aesud <- aes.ref
  aeslr$y <- aeslr$yend <- aes.ref$ymid
  aesud$x <- aesud$xend <- aes.ref$xmid
  aesud$xmid <- aesud$ymid <- aeslr$xmid <- aeslr$ymid <- NULL
  return(list(
    geom_segment(aeslr, ...),
    geom_segment(aesud, ...)
  ))
}

fct_labels <- function(stack=FALSE, stack.char = ifelse(stack,"\n"," ")) labeller(
  compartment = c(
    cases=sprintf("Symptomatic%sCases", stack.char),
    icu_p=sprintf("Critical Care%sOccupancy", stack.char),
    nonicu_p=sprintf("General Hospital%sOccupancy", stack.char),
    hosp_p=sprintf("Total Hospital%sOccupancy", stack.char),
    death_o="Deaths"
  )
)


#' @examples 
#' 
#' raw.dt <- unmitigated.addall(qread(.args[1]))
#' ps.dt <- unmitigated.peaks(raw.dt)
#' qs.dt <- qread(.args[2])
#' peakqs.dt <- ps.dt[age == "all", {
#'  pqs <- quantile(value, probs = c(.5, .75, .975))
#'  tqs <- quantile(t, probs = c(0.025, 0.25, .5))
#'  names(pqs) <- sprintf("peak.%s",c("med","hi","hi.hi"))
#'  names(tqs) <- sprintf("time.%s",c("hi.hi","hi","med"))
#'  c(as.list(pqs),as.list(tqs))
#' }, by=.(compartment)]
#' 
#' mlt <- meltquantiles(qs.dt[age == "all" & scen_id == 1])
#' ggplotqs(mlt, aes(color=variable)) +
#'   facet_grid(compartment ~ ., scale = "free_y")
#' ggplotqs(mlt, aes(fill=variable), gm = geom_area, position = "identity", alpha = 0.5) +
#'   facet_grid(compartment ~ ., scale = "free_y")
#' 
#' ggplotruns(raw.dt[age == "all"]) +
#'  facet_grid(compartment ~ ., scale = "free_y", labeller = fct_labels(TRUE)) +
#'  geom_point(data = unmitigated.peaks) +
#'  theme_minimal() +
#'  scale_x_t() +
#'  scale_y_cases()
#'  
#' ggplotruns(raw.dt[compartment == "cases"]) +
#'  facet_grid(. ~ age, scale = "free_y") +
#'  theme_minimal()
#'  
#' ggplotqs(qs.dt[scen_id == 1 & age == "all"]) +
#'  facet_grid(compartment ~ ., scale = "free_y") +
#'  geom_segment(
#'    aes(x=time.med, xend=time.hi.hi, y=peak.med, yend=peak.med, alpha="r95"),
#'    arrow = arrow(angle = 15, length=unit(6,"pt"), type="closed"),
#'    data=peakqs.dt
#'  ) +
#'  geom_segment(
#'    aes(x=time.med, xend=time.hi, y=peak.med, yend=peak.med, alpha="r50"),
#'    arrow = arrow(angle = 15, length=unit(6,"pt"), type="closed"),
#'    data=peakqs.dt
#'  ) +
#' geom_segment(
#'    aes(x=time.med, xend=time.med, y=peak.med, yend=peak.hi.hi, alpha="r95"),
#'    arrow = arrow(angle = 15, length=unit(6,"pt"), type="closed"),
#'    data=peakqs.dt
#'  ) +
#' geom_segment(
#'    aes(x=time.med, xend=time.med, y=peak.med, yend=peak.hi, alpha="r50"),
#'    arrow = arrow(angle = 45, length=unit(3,"pt"), type="closed"),
#'    data=peakqs.dt
#'  ) +
#'  scale_alpha_manual(values = c(med=0.75, r95=0.25, r50=0.5)) +
#'  coord_cartesian(xlim = c(50, 200)) +
#'  theme_minimal()


ggplot(raw.dt) + aes(t, )

scale_y_logcases