suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "mapping/regionref.rds"
) else commandArgs(trailingOnly = TRUE)

regioncodes <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"

regionref <- fread(regioncodes, na.strings = NULL)
excl <- c(5,22,37,43,45,58)
afrregions <- regionref[region %like% "Africa"][-excl][,.SD,.SDcols=c("alpha-3","sub-region","intermediate-region", "country-code")]
afrregions[`intermediate-region` == "", `intermediate-region` := `sub-region` ]
afrkey <- setkey(afrregions[,.(region = `intermediate-region`, iso=`alpha-3`, code=`country-code`)], iso)

pop.dt <- readRDS("../covidm/data/wpp2019_pop2020.rds")[country_code %in% afrkey$code]
tots.dt <- pop.dt[afrkey, on=.(country_code = code)][, .(tot = sum(f+m)*1000), by=iso]

afrkey[tots.dt, on=.(iso=iso), tot := tot ]

saveRDS(afrkey, tail(.args, 1))
