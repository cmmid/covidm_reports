require(data.table)

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/interventions"
) else commandArgs(trailingOnly = TRUE)

refs.dt <- data.table(fns = list.files(.args[1], "\\.rds", full.names = T))

refs.dt[,
  country := gsub("^.+sceni_.+_(\\w+)\\.rds", "\\1", fns)      
]

for (cty in refs.dt[, unique(country)]) {
  res <- rbindlist(lapply(refs.dt[country == cty, fns], function(fn) {
    inn <- readRDS(fn)
    inn[, sample_id := as.integer(gsub("^.+_(\\d+)_\\w+\\.rds$","\\1",fn))]
  }))
  saveRDS(res, sprintf("%s/%s_consolidated.rds", .args[1], country))
}
