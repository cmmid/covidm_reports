suppressPackageStartupMessages({
  require(rmarkdown)
  require(data.table)
  require(qs)
})

.args <- if (interactive()) c(
  sprintf("simulate/IDP/%s.qs",c("001","alls","accs","peak")),
  "report/template-rev.Rmd",
  sprintf("utils/%s.rda", c("sharedconst")),
  sprintf("plotting/%s.rda", c("plotfuns")),
  "~/Dropbox/covidm_reports/hpc_inputs",
  "~/Dropbox/covidm_reports/generation/data_contacts_missing.csv",
  "report/IDP/report.pdf"
) else commandArgs(trailingOnly = TRUE)
#' @example 
#' .args <- gsub("CPV","IDP",.args)

# first 4 args
simfns <- .args[1:4]
# second to last arg
template <- .args[5]
whichph <- 6:7
plothelpers <- .args[whichph]
fromph <- max(whichph) + 1

tarreport <- tail(.args, 1)
ctar <- basename(dirname(tarreport))

inputdir <- path.expand(.args[fromph])
detailinputdir <- sprintf("%s/%s", inputdir, ctar)

cmrefpath <- .args[fromph+1]


for (ph in plothelpers) {
  if (grepl("rda$", ph)) {
    load(ph)
  } else if (grepl("R$", ph)) {
    source(ph)
  }
}

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

unmitigated <- full.reinflate(qread(simfns[1]))
# unmitigated[, date := t + timing$day0date ]
all.quan <- qread(simfns[2])
accs.int <- qread(simfns[3])
peak.int <- qread(simfns[4])

base_params <- params_set[[1]]$pop[[1]]

if (length(contact_matrices) != 4) {
  refcm <- contact_matrices[[1]]
  for (i in 2:length(contact_matrices)) with(contact_matrices[[i]], {
    refcm$home <- refcm$home + home
    refcm$work <- refcm$work + work
    refcm$school <- refcm$school + school
    refcm$other <- refcm$other + other
  })
  refcm$home <- refcm$home/length(contact_matrices)
  refcm$work <- refcm$work/length(contact_matrices)
  refcm$school <- refcm$school/length(contact_matrices)
  refcm$other <- refcm$other/length(contact_matrices)
} else {
  refcm <- contact_matrices
}

poppyra <- with(base_params, data.table(
  pop = size,
  group = factor(
    group_names,
    levels = group_names,
    ordered = T
  )
))
loc <- base_params$name

cmref <- fread(cmrefpath)[iso == ctar]

if (!cmref$cm) {
  is_analogy <- cmref$cm_name
} else {
  is_analogy <- NA
}

reduce_ages <- function(dt) {
  fctr <- function(i, lvls = c("<14", "15-29", "30-44", "45-59", "60+")) {
    factor(
      lvls[i],
      levels = lvls, ordered = T
    )
  }
  dt[between(as.integer(group), 1, 3), age := fctr(1)]
  dt[between(as.integer(group), 4, 6), age := fctr(2)]
  dt[between(as.integer(group), 7, 9), age := fctr(3)]
  dt[between(as.integer(group), 10, 12), age := fctr(4)]
  dt[as.integer(group) >= 13, age := fctr(5)]
}

reduce_ages(poppyra)

poppyra[, per_by_group := pop / sum(pop)]
poppyra[, per_by_age := sum(pop), by = age]
poppyra[, per_by_age := per_by_age / sum(pop)]

.params <- list(
  location = loc,
  unmitigated = unmitigated,
  peaks = peak.int,
  accs = accs.int,
  alls = all.quan,
  contactmatrices = refcm,
  poppyra = poppyra,
  plothelpers = plothelpers,
  is_analogy = is_analogy
)
#' @examples 
#' params <- .params

rmarkdown::render(
  template,
  output_file = tarreport,
  params = .params
)
