suppressPackageStartupMessages({
  require(data.table)
})
# arrange interventions data

.args <- if (interactive()) c(
  "interventions/clean.csv",
  "~/Dropbox/covidm_reports/generation/data_contacts_missing.csv",
  "interventions/digested.csv"
) else commandArgs(trailingOnly = TRUE)

int.dt <- fread(.args[1])
ref.dt <- fread(.args[2])

entries <- int.dt[iso %in% ref.dt$iso][
  prov_category == "Lockdown"
][, .SD, by=country_territory_area, .SDcols = -c(
  "who_category","value_usd","percent_interest","response_type","non_compliance",
  "source_type", "source","link","source_alt","comments","area_covered","who_id",
  "who_region", "number","dataset","prop_id","prov_subcategory","date_entry"
)]

entries$prov_measure <- factor(
  entries$prov_measure,
  levels = rev(c(
    "Lockdown of refugee/idp camps or other minorities",
    "Partial lockdown",
    "Full lockdown"
  )),
  ordered = TRUE
)

entries$date_start <- as.Date(
  entries$date_start,
  "%d/%m/%Y"  
)

# TODO - "merge" responses that start / end - right now none of those
interventions <- entries[order(date_start, prov_measure),.SD[1],keyby=.(country_territory_area)]

fwrite(interventions, tail(.args, 1))