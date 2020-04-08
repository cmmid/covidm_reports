require(rmarkdown)
require(data.table)
require(data2019nCoV)

.args <- if (interactive()) c(
  "Algeria-res.rds", "report-template.Rmd", "Algeria.pdf"
) else commandArgs(trailingOnly = TRUE)

# need to get contact matrix
# need to get age pyramid

simdata <- readRDS(.args[1])
template <- .args[2]
target <- tail(.args, 1)

dynamics <- simdata$dynamics
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

# TODO look this up some how?
refday <- "Unknown"

if (any(grepl(gsub(" ",".",loc), names(WHO_SR)))) {
  
}

rmarkdown::render(
  .args[2],
  output_file = tail(.args, 1),
  params = list(
    location = loc,
    dynamics = dynamics,
    contactmatrices = contactmatrices,
    poppyra = poppyra,
    refday = refday
  )
)
