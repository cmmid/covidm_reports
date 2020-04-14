require(rmarkdown)
require(data.table)
#require(qs)
#require(data2019nCoV)

.args <- if (interactive()) c(
  "caboverde", # root path to sim outputs
  "report-template.Rmd",
  "~/Dropbox/covidm_reports/interventions/inputs",
  "~/Dropbox/covidm_reports/interventions/generation_data/lmic_early_deaths.csv",
  "~/Dropbox/covidm_reports/interventions/generation_data/data_contacts_missing.csv",
  "testpdf.pdf"
) else commandArgs(trailingOnly = TRUE)

simdata <- readRDS(.args[1])
inputdir <- path.expand(.args[3])
ctar <- tolower(gsub("-res\\.rds","",basename(.args[1])))
detailinputdir <- sprintf("%s/%s", inputdir, ctar)
refpath <- .args[4]
template <- .args[2]
target <- tail(.args, 1)


for (.fn in list.files(inputdir, "\\.rds", full.names = TRUE, include.dirs = F)) {
  .nm <- gsub(sprintf("^%s/(.+)\\.rds$",inputdir),"\\1",.fn)
  assign(.nm, readRDS(.fn))
}

if (dir.exists(detailinputdir)) {
  for (.fn in list.files(detailinputdir, "\\.rds", full.names = TRUE)) {
    .nm <- gsub(sprintf("^%s/(.+)\\.rds$", detailinputdir),"\\1",.fn)
    ## overrides previous assignment, if any
    assign(.nm, readRDS(.fn))
  }
}

ref <- fread(refpath, skip = 2, col.names = c("country", "d1", "d2", "d3"), drop = 5)
ref[d1=="", d1 := NA]
ref[d2=="", d2 := NA]
ref[d3=="", d3 := NA]
ref[, d1 := as.Date(sprintf("%s-2020",d1), format = "%d-%b-%Y") ]
ref[, d2 := as.Date(sprintf("%s-2020",d2), format = "%d-%b-%Y") ]
ref[, d3 := as.Date(sprintf("%s-2020",d3), format = "%d-%b-%Y") ]
ref[, cty := tolower(gsub("[^a-zA-Z]","",country))]

ref[cty == "caboverde", d1 := NA ] # assert this death was imported

day0 <- NA # ref[cty == ctar, d1]

if (!length(day0)) day0 <- NA

is_analogy <- NA

dynamics <- simdata$dynamics
dynamics$t <- as.integer(dynamics$t)
contactmatrices <- simdata$base_parameters$pop[[1]]$matrices
poppyra <- data.table(
  pop = simdata$base_parameters$pop[[1]]$size,
  group = factor(
    simdata$base_parameters$pop[[1]]$group_names,
    levels = simdata$base_parameters$pop[[1]]$group_names,
    ordered = T
  )
)
loc <- simdata$base_parameters$pop[[1]]$name

rmarkdown::render(
  template,
  output_file = tail(.args, 1),
  params = list(
    location = loc,
    dynamics = dynamics,
    contactmatrices = contactmatrices,
    poppyra = poppyra,
    day0 = day0,
    is_analogy = is_analogy
  )
)
