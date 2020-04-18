#' Visualise the distributions of parameter sets

library(tidyverse)
library(cowplot)

.args <- if (interactive()) c(
  "plotpars.rda"
) else commandArgs(trailingOnly = TRUE)

#' @examples
#'
#' # For creating the PDF for a Normal distribution with mean=1 and sd=10 from 2-4
#' create_distributed_data(dnorm, 2, 4, "dnorm", mean = 1, sd = 10)
#'
#' # For creating the PDF for a Gamma distribution with shape=7, rate=0.2 from 4-8
#' create_distributed_data(dgamma, 4, 8, "gamma", shape = 7, rate = 0.2)
create_distributed_data <- function(distribution_function, min_value, max_value, reference_name, ...) {
  ys <- distribution_function(seq(min_value, max_value, length.out = 1000), ...)
  output <- data.frame(ys) %>% mutate(xs = seq(min_value, max_value, length.out = 1000), reference = reference_name)
  return(output)
}

#' Arranges all the parameter plots in a grid
#'
#' @examples
#'
#' plot_all_parameters()
plot_all_parameters <- function() {
  r0_plot <- plot_r0_distribution() + labs(subtitle = "A - Basic reproductive rate") + theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  case_fatality_plot <- plot_case_fatality_ratio() + labs(subtitle = "B - Case fatality ratio") + theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  serial_interval_plot <- plot_serial_interval_ratio() + labs(subtitle = "C - Serial interval") + theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  yis_plot <- plot_yis() + labs(subtitle = "D - Age-specific clinical fraction\n") + theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  fs_plot <- plot_fs() + labs(subtitle = "E - Relative infectiousness in\n     subclinical cases") + theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  delay_symp <- plot_delay_symptom() + labs(subtitle = "F - Delay from symptom\n     onset to hospitilisation") + theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  delay_hosp_non_icu <- plot_delay_hosp_non_icu() + labs(subtitle = "G - Delay from hospitilisation to\n     discharge/death (non-ICU)") +
    theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  delay_hosp_icu <- plot_delay_hosp_icu() +
    labs(subtitle = "H - Delay from hospitilisation to\n     discharge/death (ICU)") +
    theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  delay_onset_death <- plot_delay_tot() +
    labs(subtitle = "I - Delay from onset to\n    death (ICU)") +
    theme(aspect.ratio = 1.0, plot.subtitle=element_text(size=10))
  plot_grid(r0_plot,
    case_fatality_plot,
    serial_interval_plot,
    yis_plot,
    fs_plot,
    delay_symp,
    delay_hosp_non_icu,
    delay_hosp_icu,
    delay_onset_death,
    align = "hv", ncol = 3
  )
}

#' PLots the R0 probability density function
plot_r0_distribution <- function() {
  r0_data <- create_distributed_data(distribution_function = dnorm, min_value = 0, max_value = 10, reference_name = "$R_0$", mean = 2.7, sd = 0.5)
  return(r0_data %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    xlab(expression(R[0] ~ value)) +
    ylab("Probability density"))
}

#' Plots the age-specific clinical fraction parameter as a function of age
plot_yis <- function() {
  yis_values <- data.frame(matrix(c(seq(0, 95, 5), seq(0.2, 0.1, length.out = 20)), byrow = FALSE, ncol = 2)) %>%
    rename(Age = X1, p = X2)
  return(yis_values %>% ggplot() +
    geom_line(aes(x = Age, y = p)) +
    theme_minimal() +
    ylab(expression(y[i] ~ value)))
}

#' Plots the relative infectiousness of subclinical cases as function of age
plot_fs <- function() {
  fs_values <- data.frame(matrix(c(seq(0, 95, 5), seq(0.2, 0.1, length.out = 20)), byrow = FALSE, ncol = 2)) %>%
    rename(Age = X1, p = X2)
  return(fs_values %>% ggplot() +
    geom_line(aes(x = Age, y = p)) +
    theme_minimal() +
    ylab(expression(f[i] ~ value)))
}

#' Plots the distribution of the case fatality ratio
plot_case_fatality_ratio <- function() {
  case_fatality_data <- create_distributed_data(
    distribution_function = dnorm,
    min_value = 0, max_value = 1, reference_name = "CFR", mean = 0.5, sd = 0.05
  )
  return(case_fatality_data %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    ylab("Probability density") +
    xlab("CFR value"))
}


#' Plots the serial interval
plot_serial_interval_ratio <- function() {
  serial_interval_ratio <- create_distributed_data(
    distribution_function = dnorm,
    min_value = 0,
    max_value = 10, reference_name = "SI",
    mean = 6.5, sd = 0.5
  )
  return(serial_interval_ratio %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    ylab("Probability density") +
    xlab("Serial interval value"))
}

#' Plots the delay from symptom onset to hospitilisation
plot_delay_symptom <- function() {
  delay_data <- create_distributed_data(
    distribution_function = dgamma,
    min_value = 0,
    max_value = 25, reference_name = "Delay",
    shape = 7, scale = 7 / 2.65^2
  )

  return(delay_data %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    ylab("Probability density") +
    xlab(expression(d[h] ~ value)))
}

#' Plots the delay from hospitilisation to discharge/death (non-ICU)
plot_delay_hosp_non_icu <- function() {
  delay_data <- create_distributed_data(
    distribution_function = dgamma,
    min_value = 0,
    max_value = 30, reference_name = "Delay",
    shape = 8, scale = 8 / 2.83^2
  )

  return(delay_data %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    ylab("Probability density") +
    xlab(expression(d[d] ~ value)))
}

#' Plots the delay from hospitilisation to discharge/death (ICU)
plot_delay_hosp_icu <- function() {
  delay_data <- create_distributed_data(
    distribution_function = dgamma,
    min_value = 0,
    max_value = 30, reference_name = "Delay",
    shape = 10, scale = 10 / 3.16^2
  )

  return(delay_data %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    ylab("Probability density") +
    xlab(expression(d[icu] ~ value)))
}

#' Plots the delay from onset to death (ICU)
plot_delay_tot <- function() {
  delay_data <- create_distributed_data(
    distribution_function = dgamma,
    min_value = 0,
    max_value = 40, reference_name = "Delay",
    shape = 22, scale = 22 / 4.69^2
  )

  return(delay_data %>% ggplot() +
    geom_line(aes(x = xs, y = ys)) +
    theme_minimal() +
    ylab("Probability density") +
    xlab(expression(d[tot] ~ value)))
}

save(list=ls(), file=tail(.args, 1))