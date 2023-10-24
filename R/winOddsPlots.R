#' Function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome for a hierarchical endpoint.
#'
#' Implemented for objects of type 'maraca' and 'hce'.
#'
#' @param x an object of S3 class 'maraca' or 'hce'.
#' @param \dots further arguments to be passed to the
#'        object-specific functions
#' @export
component_plot <- function(x, ...) {
  UseMethod("component_plot", x)
}

#' @export
component_plot.default <- function(x,
                                   ...) {
  paste0("component_plot() function can only handle inputs of class ",
         "'hce' or 'maraca'. Your input has class ", class(x), ".")
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome directly from a maraca object.
#' Note that for this plot, when creating the maraca object using the maraca()
#' function, the argument "compute_win_odds" has to be set to TRUE.
#' Check the vignette "Maraca Plots - Plotting win odds" for more details.
#'
#' @param x an object of S3 class 'maraca'.
#' @param \dots not used
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Plotting win odds".
#' @return Component plot as a ggplot2 object.
#' @examples
#'
#' data(hce_scenario_a)
#'
#' maraca_dat <- maraca(data = hce_scenario_a,
#'                      tte_outcomes = c("Outcome I", "Outcome II",
#'                                       "Outcome III", "Outcome IV"),
#'                      continuous_outcome = "Continuous outcome",
#'                      fixed_followup_days = 3 * 365,
#'                      column_names = c(outcome = "GROUP",
#'                                       arm = "TRTP",
#'                                       value = "AVAL0"),
#'                      arm_levels = c(active = "Active",
#'                                     control = "Control"),
#'                      compute_win_odds = TRUE
#'                      )
#'
#' component_plot(maraca_dat)
#'
#' @export
component_plot.maraca <- function(x,
                                  theme = "maraca",
                                  ...) {

  # Check that win odds were calculated for the maraca object
  if (is.null(x[["win_odds_outcome"]])) {
    stop(paste0("Win odds not calculated for maraca object.\n",
                "  Make sure to set compute_win_odds = TRUE when ",
                "creating the maraca object."))
  }

  # Get win odds by outcome from maraca object
  win_odds_outcome <- x$win_odds_outcome
  # List of outcomes in order of plotting
  endpoints <- c(x$tte_outcomes, x$continuous_outcome)
  # Create data set for potting
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints,
                                         x$arm_levels)
  # Create component plot
  plot <- .create_component_plot(wo_bar_nc, endpoints, theme)

  plot <- .add_win_odds_to_plot(plot, x$win_odds,
                                x = (length(endpoints) + 1.5),
                                y = max(wo_bar_nc$percentage) * 1.5,
                                hjust = 0.85)

  return(plot)
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome directly from an hce object.
#' Check the vignette "Maraca Plots - Plotting win odds" for more details.
#'
#' @param x an object of S3 class 'hce'.
#' @param \dots not used
#' @param continuous_outcome A single string containing the continuous
#'                           outcome label. Default value "C".
#' @param arm_levels A named vector of exactly two strings, mapping the
#'                   values used for the active and control arms to the values
#'                   used in the data. The names must be "active" and "control"
#'                   in this order. Note that this parameter only need to
#'                   be specified if you have labels different from
#'                    "active" and "control".
#' @param fixed_followup_days Not needed if HCE object contains information
#'                            on fixed follow-up days in the study
#'                            (column PADY or TTEfixed,
#'                            depending on hce version).
#'                            Otherwise, this argument must be specified.
#'                            Note: If argument is specified and HCE object
#'                            contains PADY or TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Plotting win odds".
#' @return Component plot as a ggplot2 object.
#' @examples
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
#'              seed = 31337)
#'
#' component_plot(hce_dat)
#' @export
#'
component_plot.hce <- function(x, continuous_outcome = "C",
                               arm_levels = c(active = "A", control = "P"),
                               fixed_followup_days = NULL,
                               theme = "maraca",
                               ...) {

  # Create maraca object
  maraca_dat <- .maraca_from_hce_data(x, continuous_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds = TRUE)

  # Get win odds by outcome from maraca object
  win_odds_outcome <- maraca_dat$win_odds_outcome
  # List of outcomes in order of plotting
  endpoints <- c(maraca_dat$tte_outcomes, maraca_dat$continuous_outcome)
  # Create data set for potting
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints,
                                         maraca_dat$arm_levels)
  # Create component plot
  plot <- .create_component_plot(wo_bar_nc, endpoints, theme)

  plot <- .add_win_odds_to_plot(plot, maraca_dat$win_odds,
                                x = (length(endpoints) + 1.5),
                                y = max(wo_bar_nc$percentage) * 1.4,
                                hjust = 0.85)

  return(plot)
}
