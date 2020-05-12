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

entries <- int.dt[iso %in% ref.dt$iso][, .SD, by=country_territory_area, .SDcols = -c(
  "value_usd","percent_interest","response_type",
  "source","link","source_alt","area_covered","who_id","non_compliance",
  "who_region", "number","dataset","prop_id","prov_subcategory","date_entry"
)]

entries$date_start <- as.Date(
  entries$date_start,
  "%d/%m/%Y"  
)

subentries <- entries[
  (prov_category %in% c("Lockdown","Social distancing")) |
  ((prov_category == "") & (who_category == "Social and physical distancing measures"))
]

subentries$prov_measure <- factor(
  subentries$prov_measure,
  levels = rev(c(
    "",
    "Lockdown of refugee/idp camps or other minorities",
    "Partial lockdown",
    "Full lockdown"
  )),
  ordered = TRUE
)

# TODO - "merge" responses that start / end - right now none of those
interventions <- subentries[order(date_start, prov_measure),.SD[1],keyby=.(country_territory_area)]

fwrite(interventions, tail(.args, 1))
