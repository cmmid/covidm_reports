suppressPackageStartupMessages({
  require(data.table)
})
#' Create a list of LMIC locations
#' 
#' get the data from contact matrices

.args <- if (interactive()) c(
  "..", "LMIC.txt" ## input path, output file
) else commandArgs(trailingOnly = TRUE)

cm_path = .args[1]
# location_type == 4 => countries

afr.countries <- c(
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

ref_cm <- readRDS(sprintf("%s/data/all_matrices.rds", cm_path))
afr.countries <- intersect(names(ref_cm), afr.countries)

ref_pop <- readRDS(sprintf("%s/data/wpp2019_pop2020.rds", cm_path))[
  location_type == 4
][name %in% afr.countries]

afr.countries <- intersect(ref_pop[, as.character(unique(name))], afr.countries)

write.table(afr.countries, file=tail(.args, 1), row.names = F, col.names = F, quote = F)




