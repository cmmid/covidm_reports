#' Visualise the distributions of parameter sets
#'
#'


library(tidyverse)


#' @examples
#' 
#' # For creating the PDF for a gaussian with mean=1 and sd=10 from 2-4
#' create_distributed_data(dnorm, 2, 4, "dnorm", mean=1, sd=10)
create_distributed_data <- function(distribution_function, min_value, max_value, reference_name, ...) {
  ys <- distribution_function(seq(min_value, max_value, length.out = 1000), ...)
  output <- data.frame(ys) %>% mutate(xs = seq(min_value, max_value, length.out = 1000), reference = reference_name)
  return(output)
}

#' Arranges all the parameter plots in a grid
plot_all_parameters <- function(){
  r0_plot <- plot_r0_distribution() + labs(subtitle="A - Basic reproductive rate")
  yis_plot <- plot_yis()+ labs(subtitle="B - Age-specific clinical fraction")
  fs_plot <- plot_fs()+ labs(subtitle="C - Relative infectiousness in subclinical cases")
  case_fatality_plot <- plot_case_fatality_ratio()+ labs(subtitle="D - Case fatality ratio")
  serial_interval_plot <- plot_serial_interval_ratio()+ labs(subtitle="E - Serial interval")
  plot_grid(r0_plot,
            yis_plot,
            fs_plot,
            case_fatality_plot, 
            serial_interval_plot, align="hv", ncol=3)
  
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
plot_serial_interval_ratio <- function(){
  serial_interval_ratio <- create_distributed_data(
    distribution_function = dnorm,
    min_value = 0, 
    max_value = 1, reference_name = "SI",
    mean = 6.5, sd = 0.5
  )
  return(serial_interval_ratio %>% ggplot() +
           geom_line(aes(x = xs, y = ys)) +
           theme_minimal() +
           ylab("Probability density") +
           xlab("Serial interval value"))
}
