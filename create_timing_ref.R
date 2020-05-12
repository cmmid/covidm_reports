#' default simulation parameters
suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/generation/data_contacts_missing.csv",
  "data/interventions/digested.csv",
  "data/introductions/digested.csv",
  "caboverde",
  "~/Dropbox/covidm_reports/hpc_inputs/caboverde/timing.rds"
) else commandArgs(trailingOnly = TRUE)
#' @examples 
#' .args <- gsub("zimbabwe","guineabissau",.args)
#' .args <- gsub("zimbabwe","palestine",.args)

reference = fread(.args[1])
inttiming = fread(.args[2], colClasses = c(date_start="Date"))
day0timing = fread(.args[3], colClasses = c(date="Date"))

target = tail(.args, 2)[1]
outfile = tail(.args, 1)

namenorm <- function(n) gsub(" ","",gsub("[^a-zA-Z]","",tolower(as.character(n))))

refiso <- reference[namenorm(name) == target, iso]

intro0date <- day0timing[iso==refiso, date]
if (!length(intro0date)) intro0date <- NA

int0date <- inttiming[iso==refiso, date_start]
if (!length(int0date)) int0date <- NA

int0day <- if(!is.na(int0date)) max(as.integer(int0date - intro0date), 0) else NA_integer_

#general population parameters
timinginfo <- list(
  day0date = intro0date,
  int0day = int0day
)

saveRDS(timinginfo, outfile)

