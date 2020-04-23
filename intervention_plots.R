suppressPackageStartupMessages({
  require(qs)
  require(data.table)
  require(ggplot2)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/interventions/inputs/scenarios.rds",
  "~/Dropbox/covidm_reports/interventions/inputs/scenarios_overview.rds",
  "intplots.rda"
) else commandArgs(trailingOnly = TRUE)

scendata <- readRDS(.args[1])
scenkey <- readRDS(.args[2])

# w20 <- scendata[[3]][, which(gen_socdist_other == 0.8 & gen_socdist_stop == 9)]
# w40 <- scendata[[3]][, which(gen_socdist_other == 0.6 & gen_socdist_stop == 9)]
# sheild80 <- scendata[[4]][, which(hirisk_prop_isolated == 0.8 & hirisk_lorisk_contact == 0.2 & hirisk_contact == 1.0)]
# sheildplusdist <- scendata[[5]][,which(hirisk_prop_isolated == 0.8 & hirisk_lorisk_contact == 0.2 & hirisk_contact == 1.0)]
# 
# scens <- c(
#   scenkey[scen == 3 & s %in% c(w20, w40), index],
#   scenkey[scen == 4 & s == sheild80, index],
#   scenkey[scen == 5 & s %in% sheildplusdist, index]
# )

scens <- 2:6

source("./plotting_support.R")

int.factorize <- function(s) factor(s, c(1, scens), c(
  "Unmitigated",
  "20% distancing",
  "50% distancing",
  "80% shielding",
  "80% shielding &\n20% distancing",
  "80% shielding &\n50% distancing"
))

int.fct <- facet_grid(
  compartment ~ int.factorize(scen_id),
  scales="free", labeller = fct_labels(stack = TRUE)
)

int.plotter <- function(
  dt, whichm,
  ylab = c(
    effectiveness = "% of Negative Outcomes Averted",
    reduction = "Negative Outcomes Averted"
  )[whichm],
  ylim = switch(whichm, effectiveness = c(0, 1), reduction = c(0, NA))
) ggplot(dt[
  metric == whichm &
  age == "all" &
  compartment %in% c("cases","death_o","hosp_p")
]) + aes(t, med) +
  geom_ribbon(
    aes(ymin=lo95, ymax=hi95),
    alpha=0.2
  ) +
  geom_ribbon(
    aes(ymin=lo50, ymax=hi50),
    alpha=0.25
  ) +
  geom_line(size=1) + 
  int.fct +
  scale_x_continuous(
    "Months Since COVID-19 Introduction", breaks = c(30, 60, 90, 180),
    labels = function(bs) bs/30,
    expand = c(0, 0)
  ) + scale_y_continuous(ylab) +
  coord_cartesian(xlim=c(0, 180), ylim = ylim, expand = F) +
  theme_minimal() + theme(
    panel.grid.minor=element_blank(),
    panel.spacing = unit(12, "pt"),
    legend.position="none"
  ) 

int.peaks <- function(
  dt, whichm = "effectiveness",
  ylab = c(
    effectiveness = "% Peak Reduction",
    reduction = "Peak Reduction"
  )[whichm],
  ylim = switch(whichm, effectiveness = c(0, 1), reduction = c(0, NA))
) ggplot(
    dt[
      age == "all" &
      metric == whichm &
      compartment %in% c("cases","death_o","hosp_p")
    ]
  ) +
  aes(int.factorize(scen_id), med, color=compartment) +
  geom_linerange(aes(ymin=lo95, ymax=hi95), position = position_dodge(width=0.5)) +
  geom_linerange(aes(ymin=lo50, ymax=hi50), position = position_dodge(width=0.5), size = 2) +
  coord_cartesian(ylim=ylim, expand = F) +
  scale_y_continuous(ylab) +
  scale_color_manual(
    "Outcome",
    labels = c(cases="Symptomatic Case", hosp_p="Hospital Bed Occupancy", death_o="Deaths"),
    values = c(cases="goldenrod", hosp_p = "firebrick", death_o="black")
  ) + theme_minimal() + theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    axis.title.x = element_blank()
  )

int.peaks.all <- function(dt, override.aes = aes(), range.scale = c(1,2,3)) {
  ggplot(dt) +
    aes(factor(scen_id), med, color=int.factorize(scen_id)) + override.aes +
    geom_linerange(aes(ymin=lo.lo, ymax=hi.hi, alpha="hi.hi"), position = position_dodge(width=0.5), size = range.scale[1]) +
    geom_linerange(aes(ymin=lo, ymax=hi, alpha="hi"), position = position_dodge(width=0.5), size = range.scale[2]) +
    geom_point(aes(y=med, alpha="med"), position = position_dodge(width=0.5), size = range.scale[3]) +
    scale_color_discrete(NULL) +
    scale_y_continuous("Peak Values") +
    coord_cartesian(ylim = c(0, NA), expand = F)
}

save(
  scens, int.peaks, int.plotter, int.fct, int.factorize,
  int.peaks.all,
  file = tail(.args, 1)
)

#' @examples 
#' dt <- qread("caboverde/accs.qs")[scen_id %in% scens]
#' int.plotter(dt, "effectiveness")
#' int.plotter(dt, "reduction")
#' 
#' ref.dt <- qread("caboverde/001.qs")
#' dt <- qread("caboverde/peak.qs")[scen_id %in% scens]
#' int.peaks(dt, whichm = "reduction") + facet_grid(compartment ~ ., scales = "free_y")
#' 
#' refp <- unmitigated.peaks(unmitigated.addall(ref.dt))
#' qpeaks <- refp[age == "all", {
#'   qs <- quantile(value, probs = refprobs)
#'   names(qs) <- names(refprobs)
#'   as.list(qs)
#' }, by=.(age, compartment)]
#' 
#' join.dt <- rbind(dt[
#'   metric == "reduction"
#' ][qpeaks, on=.(age, compartment)][,
#'   .(scen_id, compartment, age, lo95 = i.med-lo95, lo50 = i.med-lo50, med = i.med-med, hi50=i.med-hi50, hi95=i.med-hi95)
#' ], qpeaks[,.(scen_id=1, compartment, age, lo95=lo.lo, lo50=lo, med, hi50=hi, hi95=lo.lo)])
#' 
#' a.dt <- setkey(
#'  qread("caboverde/alls.qs")[scen_id %in% c(1, scens)],
#'  compartment, age, t, scen_id
#' )
#' 
#' ggplotqs(meltquantiles(a.dt[age=="all"]), aes(
#'   color=factor(scen_id), group=variable, alpha=variable
#' )) +
#'   facet_grid(compartment ~ scen_id, scale = "free_y", switch = "y", labeller = fct_labels(
#'   scen_id = { res <- levels(int.factorize(c(1, scens))); names(res) <- c(1, scens); res }
#' )) + 
#' scale_x_t() +
#' scale_alpha_manual(guide = "none", values = c(lo.lo=0.25, lo=0.5, med=1, hi=0.5, hi.hi=0.25)) +
#' scale_color_discrete(guide = "none") +
#' theme_minimal() + theme(
#'   strip.placement = "outside",
#'   axis.title.y = element_blank()
#' )
#' 
#' 