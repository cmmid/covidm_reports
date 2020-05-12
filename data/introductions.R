suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/generation/data_contacts_missing.csv",
  "introductions/digested.csv"
) else commandArgs(trailingOnly = TRUE)

ref <- fread(.args[1])

ecdc <- fread(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
)

ecdc[, date := as.Date(dateRep, "%d/%m/%Y")]

firstreports <- ecdc[
  countryterritoryCode %in% ref$iso
][
  (cases+deaths)!=0
][order(date),.SD[1],by=.(iso=countryterritoryCode)]
#' @example 
#' missing <- setdiff(ref$iso, firstreports$iso)

fwrite(firstreports, tail(.args, 1))