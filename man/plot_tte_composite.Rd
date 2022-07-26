% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maraca.R
\name{plot_tte_composite}
\alias{plot_tte_composite}
\title{Creates and returns the tte composite plot of the maraca data.}
\usage{
plot_tte_composite(obj)
}
\arguments{
\item{obj}{an object of S3 class 'maraca'}
}
\value{
a ggplot2 object of the data. This function
will not render the plot immediately. You have to print() the returned
object for it to be displayed.
}
\description{
Creates and returns the tte composite plot of the maraca data.
}
\examples{
data(hce_scenario_a)
hce_test <- maraca(
  data = hce_scenario_a,
  tte_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
  continuous_outcome = "Continuous outcome",
  column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
  arm_levels = c(active = "Active", control = "Control"),
  compute_win_odds = TRUE
)
plot <- plot_tte_composite(hce_test)
}
