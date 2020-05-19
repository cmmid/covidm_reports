suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_hpc_output", "all_peaks.csv"
) else commandArgs(trailingOnly = TRUE)

fls <- sort(grep(
  "old",
  list.files(.args[1], "001.qs", recursive = TRUE, full.names = TRUE),
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
  res <- qread(fn)[, .(value = sum(value)), by=.(run, t, compartment)]
  pk <- res[,{
    ind <- which.max(value)
    .(t=t[ind], value=value[ind])
  },by=.(run, compartment)]
  qpk <- pk[,{
    qsv <- quantile(value, refprobs)
    qsv <- c(down(qsv[1]),down(qsv[2]),up(qsv[3]),up(qsv[4]))
    names(qsv) <- sprintf("%sv",names(refprobs))
    qst <- quantile(t, rev(refprobs))
    qst <- c(down(qst[1]),down(qst[2]),up(qst[3]),up(qst[4]))
    names(qst) <- sprintf("%st",names(refprobs))
    c(as.list(qsv), as.list(qst))
  },by=compartment]
  qpk[, fl := fn ]
}))

allpks[, country := gsub("^.+/([^/]+)/001.qs$","\\1", fl) ]
allpks$fl <- NULL
fwrite(allpks, tail(.args, 1), scipen = 10)

#' @examples 
#' require(ggplot2)
#' oldres <- fread("old_peaks.csv")[, gen := factor("V1", levels = c("V1","V2"), ordered = TRUE) ]
#' newres <- fread("all_peaks.csv")[, gen := factor("V2", levels = c("V1","V2"), ordered = TRUE) ]
#' popref <- rbindlist(lapply(
#'   list.files("~/Dropbox/covidm_reports/hpc_inputs","params_set\\.rds$", recursive = TRUE, full.names = TRUE),
#'   function(fn) { res <- data.table(pop = readRDS(fn)[[1]]$pop[[1]]$size); res[, fn := fn ] }
#' ))[, country := gsub("^.+/([^/]+)/params_set\\.rds$","\\1", fn) ]
#' 
#' allres <- rbind(oldres, newres)[popref, on=.(country), pop := pop ]
#' 
#' unmitp <- ggplot(allres) + aes(country, color = gen, alpha = 0.5) +
#'   facet_grid(compartment ~ ., scale = "free_y", labeller = labeller(
#'     compartment = c(
#'       cases="Peak Cases",
#'       E="Peak Infections",
#'       icu_p="Critical Hospital\nDemand",
#'       nonicu_p="General Hospital\nDemand",
#'       death_o="Total Deaths"
#'     )
#'   )) +
#'   theme_minimal(base_size=7) + theme(
#'     axis.text.x = element_text(angle = 90),
#'     axis.title.x = element_blank(),
#'     legend.position = c(0.05, 0.975),
#'     legend.justification = c(0, 1),
#'     legend.direction = "horizontal"
#'   ) +
#'   geom_linerange(aes(ymin=lo50v/pop,ymax=hi50v/pop), size = 1, position = position_dodge(width=.1)) +
#'   geom_linerange(aes(ymin=lo95v/pop,ymax=hi95v/pop), size = 0.5, position = position_dodge(width=.1)) +
#'   scale_y_continuous("Per Capita Outcomes") +
#'   scale_color_discrete("Model Version") +
#'   scale_alpha(guide = "none")
#' require(cowplot)
#' save_plot("unmit_comparison.jpg", unmitp, base_width = 7, base_height = 5, dpi=600)
#' 