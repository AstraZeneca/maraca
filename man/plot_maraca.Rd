% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maraca.R
\name{plot_maraca}
\alias{plot_maraca}
\title{Creates and returns the plot of the maraca data.}
\usage{
plot_maraca(
  obj,
  continuous_grid_spacing_x = 10,
  trans = "identity",
  density_plot_type = "default",
  vline_type = "median"
)
}
\arguments{
\item{obj}{an object of S3 class 'maraca'}

\item{continuous_grid_spacing_x}{The spacing of the x grid to use for the
continuous section of the plot.}

\item{trans}{the transformation to apply to the data before plotting.
The accepted values are the same that ggplot2::scale_x_continuous}

\item{density_plot_type}{which type of plot to display in the continuous
part of the plot. Options are "default", "violin", "box", "scatter".}

\item{vline_type}{what the vertical lines in the continuous part of the plot
should highlight. Options are "median", "mean", "none".}
}
\value{
a ggplot2 object of the data. This function
will not render the plot immediately. You have to print() the returned
object for it to be displayed.
}
\description{
Creates and returns the plot of the maraca data.
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
plot <- plot_maraca(hce_test)
}
