% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maraca.R
\name{plot.maraca}
\alias{plot.maraca}
\title{Generic function to plot the maraca object using plot().}
\usage{
\method{plot}{maraca}(
  x,
  continuous_grid_spacing_x = 10,
  trans = "identity",
  density_plot_type = "default",
  vline_type = "median",
  ...
)
}
\arguments{
\item{x}{an object of S3 class 'maraca'}

\item{continuous_grid_spacing_x}{The spacing of the x grid to use for the
continuous section of the plot.}

\item{trans}{the transformation to apply to the data before plotting.
The accepted values are the same that ggplot2::scale_x_continuous}

\item{density_plot_type}{The type of plot to use to represent the density.
Accepts "default", "violin", "box" and "scatter".}

\item{vline_type}{what the vertical dashed line should represent. Accepts
"median", "mean", "none".}

\item{\dots}{not used}
}
\value{
Used for side effect. Plots the maraca object.
}
\description{
This will produce the plot_maraca plot.
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
plot(hce_test)

}
