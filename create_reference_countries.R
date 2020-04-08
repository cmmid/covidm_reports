suppressPackageStartupMessages({
  require(data.table)
})
#' Create a list of LMIC locations
#' 
#' get the data from contact matrices

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/interventions/generation_data/data_contacts_missing.csv",
  "../covidm",
  "LMIC.txt" ## input path, output file
) else commandArgs(trailingOnly = TRUE)

ref <- fread(.args[1])
cm_path = .args[2]

# location_type == 4 => countries

lmic.countries <- c(
  "Algeria", "Angola", "Benin", "Botswana", "Burkina Faso",
  "Burundi", "Cabo Verde", "Cameroon", "Central African Republic",
  "Chad", "Comoros", "Congo", "Cote d'Ivoire",
  "Dem. Republic of the Congo", "Djibouti", "Egypt",
  "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon",
  "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho",
  "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania",
  "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger",
  "Nigeria", "Reunion", "Rwanda", "Senegal", "Seychelles",
  "Sierra Leone", "Somalia", "South Africa", "South Sudan",
  "Sudan", "Tunisia", "Uganda", "United Republic of Tanzania",
  "Western Sahara", "Zambia", "Zimbabwe", "Cambodia"
)

ref_pops <- readRDS(sprintf("%s/data/wpp2019_pop2020.rds", cm_path))
missing_pop <- setdiff(lmic.countries, ref_pops[, as.character(unique(name))])

ref_cm <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))
missing_cm <- setdiff(lmic.countries, names(ref_cm))
noreplacements <- setdiff(missing_cm, ref$name)


fnl.countries <- setdiff(lmic.countries, unique(c(noreplacements, missing_pop)))

write.table(fnl.countries, file=tail(.args, 1), row.names = F, col.names = F, quote = F)




