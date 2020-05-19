suppressPackageStartupMessages({
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_hpc_output"
) else commandArgs(trailingOnly = TRUE)

fls <- grep(
  "old",
  sort(list.files(.args[1], "qs$", full.names = TRUE, recursive = TRUE)),
  invert = T, value = T
)

ls <- lapply(fls, function(fn) {
  fwrite(qread(fn), gsub("qs$","csv",fn))
})

