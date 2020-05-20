suppressPackageStartupMessages({
  require(data.table)
})

.args <- if (interactive()) c(
  "~/Dropbox/covidm_reports/generation/data_contacts_missing.csv",
  "introductions/digested.csv"
) else commandArgs(trailingOnly = TRUE)

ref <- fread(.args[1])

ecdc <- fread(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = NULL
)

ecdc[, date := as.Date(dateRep, "%d/%m/%Y")]

relevant <- ecdc[
  countryterritoryCode %in% ref$iso
][order(geoId, date)]

aligned <- relevant[
  order(date),
  cumcases := cumsum(cases),
  by=geoId
][,
  if (max(cumcases)>9) .SD[which.max(cumcases>9):.N],
  by=geoId
][, day0 := date[1], by=geoId ]

ggplot(aligned) + aes(date-day0, log10(cumcases), group=geoId) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous("Days Since Reporting 10 cases") +
  scale_y_log10()

bilin <- function (x,y) {
  #' model is
  #' y = b + ml*(Cx-x) + mr*(x-Cx)
  f <- function (Cx) {
    lhs <- function(x) ifelse(x < Cx,Cx-x,0)
    rhs <- function(x) ifelse(x < Cx,0,x-Cx)
    fit <- lm(y ~ lhs(x) + rhs(x))
    c(
      summary(fit)$r.squared, summary(fit)$coef[1], summary(fit)$coef[2], summary(fit)$coef[3]
    )
  }
  
  r2 <- function(x) -(f(x)[1])
  
  res <- optimize(r2,interval=c(min(x),max(x)))
  res <- c(res$minimum, f(res$minimum))
  return(res)
}

bl <- aligned[, if (.N >= 5) {
  res <- bilin(date-day0, cumcases)
  .(bend=res[1], slope1 = -res[4], slope2 = res[5], day0 = as.Date(day0[.N], "1970-01-01"))
}, by=.(geoId,iso3=countryterritoryCode) ]

mean(bl[, slope1/slope2 ] < 1)

interventions.ref <- fread("data/interventions/digested.csv")

bl[interventions.ref, on=.(iso3=iso), date_int := date_start ]
bl[, int := as.Date(date_int) - day0]

firstreports <- ecdc[
  countryterritoryCode %in% ref$iso
][
  (cases+deaths)!=0
][order(date),.SD[1],by=.(iso=countryterritoryCode)]
#' @example 
#' missing <- setdiff(ref$iso, firstreports$iso)

fwrite(firstreports, tail(.args, 1))