suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_hpc_output", "all_eff.csv"
) else commandArgs(trailingOnly = TRUE)

fls <- sort(grep(
  "old",
  list.files(.args[1], "accs.qs", recursive = TRUE, full.names = TRUE),
  invert = TRUE, value = TRUE
))

refprobs <- c(lo95=.025, lo50=0.25, hi50=0.75, hi95=0.975)

up <- function(v, sf = 2) {
  if (length(v)) {
    if (v>0) {
      ref <- max(floor(log10(v))-(sf-1),0)
      ceiling(v/(10^ref))*(10^ref)
    } else if (v<0) {
      -down(-v, sf)
    } else v
  } else v
}

down <- function(v, sf = 2) {
  if (length(v)) {
    if (v>0) {
      ref <- max(floor(log10(v))-(sf-1),0)
      floor(v/(10^ref))*(10^ref)
    } else if (v<0) {
      -up(-v, sf)
    } else v
  } else v
}


allpks <- rbindlist(lapply(fls, function(fn) {
  res <- qread(fn)[metric == "effectiveness" & age == "all" & t == 360]
  res[, fl := fn ]
}))

allpks[, country := gsub("^.+/([^/]+)/accs.qs$","\\1", fl) ]
allpks$fl <- NULL
fwrite(allpks, tail(.args, 1), scipen = 10)

#' @examples 
#' require(ggplot2)
#' oldres <- fread("old_eff.csv")[scen_id %in% scens][, gen := factor("V1", levels = c("V1","V2"), ordered = TRUE) ]
#' newres <- fread("all_eff.csv")[scen_id %in% scens][, gen := factor("V2", levels = c("V1","V2"), ordered = TRUE) ]
#' effp <- ggplot(rbind(oldres, newres)) + aes(gen, med, group=country) +
#'   facet_grid(
#'     compartment ~ int.factorize(scen_id),
#'     labeller = labeller(compartment = c(
#'       cases="Total Cases",
#'       E="Total Infections",
#'       hosp_p="Overall Hospital\nPerson-Time",
#'       icu_p="Critical\nPerson-Time",
#'       nonicu_p="General\nPerson-Time",
#'       death_o="Total Deaths")
#'     )
#'   ) +
#'   theme_minimal(base_size=7) + theme(
#'     axis.text.x = element_text(angle = 90),
#'     panel.spacing = unit(12,"pt")
#'   ) +
#'   coord_cartesian(ylim = c(0,1), expand = FALSE) +
#'   scale_y_continuous("Median Intervention Effectiveness 12 Months after Introduction") +
#'   scale_x_discrete(NULL) +
#'   geom_line(alpha = 0.1)
#' require(cowplot)
#' save_plot("eff_comparison.jpg", effp, base_width = 7, base_height = 5, dpi=600)
#' 