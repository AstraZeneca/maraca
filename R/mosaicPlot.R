#' Mosaic plot
#'
#' Generic function to create a mosaic plot that compares outcomes between an
#' active treatment group and a control group, highlighting areas of "Wins",
#' "Losses" and "Ties" based on endpoint hierarchy.
#'
#' Implemented for objects of type 'maraca' and 'hce'.
#'
#' Check the vignette "Maraca Plots - Introduction to the Mosaic plot"
#' for more details.
#'
#' @param x an object of S3 class 'maraca' or 'hce'.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "color1", "color2" and "none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Introduction to the Mosaic plot".
#' @param highlight_ties Flag to indicate if component ties should be
#'                       highlighted using lighter colors.
#'                       Default value: FALSE
#' @param win_prob Flag to indicate if winning probability should be shown
#'                 within the plot. Note that in order to display the
#'                 winning probability, you need to have set the
#'                 "compute_win_odds" to TRUE when creating the maraca
#'                 object.
#'                 Default value: FALSE
#' @param diagonal_line Flag to indicate if diagonal line showing an even
#'                      Win/Loss split should be displayed.
#'                      Default value: TRUE
#' @param step_outcomes A vector of strings containing the outcome labels
#'                      for all outcomes displayed as part of the step function
#'                      on the left side of the plot.
#'                      The order is kept for the plot.
#'                      By default (when set to NULL) this is automatically
#'                      updated by taking the non-continuous outcomes from
#'                      the GROUP variable in alphabetical order.
#' @param last_outcome A single string containing the last outcome label
#'                     displayed on the right side of the plot.
#'                     Default value "C".
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
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param \dots not used
#' @return Mosaic plot as a ggplot2 object.
#' @examples
#'
#' data(hce_scenario_a)
#'
#' maraca_dat <- maraca(data = hce_scenario_a,
#'                      step_outcomes = c("Outcome I", "Outcome II",
#'                                       "Outcome III", "Outcome IV"),
#'                      last_outcome = "Continuous outcome",
#'                      fixed_followup_days = 3 * 365,
#'                      column_names = c(outcome = "GROUP",
#'                                       arm = "TRTP",
#'                                       value = "AVAL0"),
#'                      arm_levels = c(active = "Active",
#'                                     control = "Control"),
#'                      compute_win_odds = TRUE
#'                      )
#'
#' mosaic_plot(maraca_dat)
#'
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
#'              seed = 31337)
#'
#' mosaic_plot(hce_dat)
#'
#' @export
mosaic_plot <- function(x, ...) {
  UseMethod("mosaic_plot", x)
}

#' @rdname mosaic_plot
#' @export
mosaic_plot.default <- function(x,
                                ...) {
  paste0("mosaic_plot() function can only handle inputs of class ",
         "'hce' or 'maraca'. Your input has class ", class(x), ".")
}

#' @rdname mosaic_plot
#' @export
mosaic_plot.maraca <- function(x,
                               theme = "maraca",
                               highlight_ties = FALSE,
                               win_prob = FALSE,
                               diagonal_line = TRUE,
                               ...) {

  aes <- ggplot2::aes

  # Check if highlight_ties is flag
  checkmate::assert_flag(highlight_ties)
  # Check if win_prob is flag
  checkmate::assert_flag(win_prob)
  # Check if diagonal_line is flag
  checkmate::assert_flag(diagonal_line)

  if (win_prob && is.null(x$win_odds_outcome)) {
    stop(paste("In order to display the winning probabilities in the plot",
               "(win_prob = TRUE), the maraca object needs to have been",
               "created using the flag compute_win_odds = TRUE"))
  }
  # Names of endpoints for plotting
  endpoints <- c(x$step_outcomes, x$last_outcome)

  # Divide by arm
  arms <- x$arm_levels
  ecdf_df <-   x$ecdf_by_outcome$data
  ecdf_act <- ecdf_df[ecdf_df$arm == arms["active"], ]
  ecdf_ctrl <- ecdf_df[ecdf_df$arm == arms["control"], ]
  # Step function ecdf part used for mosaic plot
  steps_act <- stats::stepfun(ecdf_act$adjusted.time,
                              c(0, ecdf_act$step_values))
  steps_ctrl <- stats::stepfun(ecdf_ctrl$adjusted.time,
                               c(0, ecdf_ctrl$step_values))
  # Create grid to plot over
  x_grid <- c(0, sort(unique(ecdf_df$adjusted.time)))
  # Steps at same time point - active vs control
  act_steps <- c(0, steps_act(x_grid) / 100)
  ctrl_steps <- c(0, steps_ctrl(x_grid) / 100)

  # Last outcome
  last_df <- x$data_last_outcome$data
  if (x$last_type == "continuous") {
    # Make sure if lower is better that opposite scaling is done - 1 for lowest.
    if (x$lowerBetter) {
      last_df$x <- 100 - last_df$x
    }
    last_act <- last_df[last_df$arm == arms["active"], ]
    last_ctrl <- last_df[last_df$arm == arms["control"], ]
    # Create grid to plot over
    x_grid <- c(sort(unique(last_df$x)))
    # ECDF over last outcome (make sure to scale to right range)
    act_last <- stats::ecdf(last_act$x)(x_grid) * (1 - max(act_steps)) +
      max(act_steps)
    ctrl_last <- stats::ecdf(last_ctrl$x)(x_grid) * (1 - max(ctrl_steps)) +
      max(ctrl_steps)
  } else {
    act_last <- c(max(act_steps), 1)
    ctrl_last <- c(max(ctrl_steps), 1)
  }

  # Combine to step and last outcomes to 1 line
  act_line <- sort(c(act_steps, act_last))
  ctrl_line <- sort(c(ctrl_steps, ctrl_last))

  # Help variables indicating the cumulative proportions of each arm/endpoint
  nums_act <- unlist(x$meta[, 9], use.names = FALSE)
  nums_ctrl <- unlist(x$meta[, 10], use.names = FALSE)
  props_act <- nums_act / sum(nums_act)
  props_ctrl <- nums_ctrl / sum(nums_ctrl)
  cum_props_act <- cumsum(props_act)
  cum_props_ctrl <- cumsum(props_ctrl)

  # Ticks position for endpoint labeling
  act_ticks <- cum_props_act - props_act / 2
  ctrl_ticks <- cum_props_ctrl - props_ctrl / 2

  # Mosaic plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1,
                           fill = "Win")) +
    ggplot2::geom_area(aes(x = ctrl_line, y = act_line, fill = "Loss")) +
    ggplot2::geom_hline(yintercept = cum_props_act, color = "white") +
    ggplot2::geom_vline(xintercept = cum_props_ctrl, color = "white") +
    ggplot2::geom_line(aes(x = ctrl_line, y = act_line), color = "white",
                       linewidth = 0.5) +
    ggplot2::geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "black",
                          linewidth = 0.5) +
    ggplot2::labs(fill = "Results") +
    ggplot2::scale_x_continuous(name = paste(arms["control"], "proportions"),
                                breaks = ctrl_ticks, labels = endpoints,
                                minor_breaks = NULL, limits = c(0, 1),
                                expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(name = paste(arms["active"], "proportions"),
                                breaks = act_ticks, labels = endpoints,
                                minor_breaks = NULL, limits = c(0, 1),
                                expand = ggplot2::expansion(0)) +
    ggplot2::coord_fixed(clip = "off")

  # Add highlighted ties if flag is TRUE
  if (highlight_ties) {
    plot <- plot +
      ggplot2::geom_rect(aes(xmin = c(0, utils::head(cum_props_ctrl, -1)),
                             xmax = cum_props_ctrl,
                             ymin = c(0, utils::head(cum_props_act, -1)),
                             ymax = cum_props_act), fill = "white",
                         alpha = 0.5) +
      ggplot2::labs(caption = "Ties highlighted in lighter color")
  }

  if (win_prob) {
    win_prob <- paste0("Win probability: ",
                       100 * round(x$win_odds_outcome$WO["WP"], 3), "%")
    plot <- plot +
      ggplot2::geom_label(aes(x = 0.05, y = 0.95, label = win_prob), vjust = 1,
                          hjust = 0, size = 3,
                          label.padding = ggplot2::unit(0.7, "lines"))
  }

  if (diagonal_line) {
    plot <- plot +
      ggplot2::geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                            colour = "white", linetype = 2)
  }

  # Add styling
  plot <- switch(theme,
                 "maraca" = .theme_maraca_mosaic(plot),
                 "color1" = .theme_color1_mosaic(plot),
                 "color2" = .theme_color2_mosaic(plot),
                 "none" = plot,
                 stop("Please provide theme that exists"))

  return(plot)
}



#' @rdname mosaic_plot
#' @export
mosaic_plot.hce <- function(x, step_outcomes = NULL,
                            last_outcome = "C",
                            arm_levels = c(active = "A", control = "P"),
                            fixed_followup_days = NULL,
                            theme = "maraca",
                            highlight_ties = FALSE,
                            win_prob = FALSE,
                            diagonal_line = TRUE,
                            lowerBetter = FALSE,
                            ...) {

  # Create maraca object
  maraca_dat <- .maraca_from_hce_data(x, step_outcomes,
                                      last_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds = TRUE,
                                      lowerBetter = lowerBetter)

  plot <- mosaic_plot(maraca_dat,
                      theme = theme,
                      highlight_ties = highlight_ties,
                      win_prob = win_prob,
                      diagonal_line = diagonal_line)

  return(plot)
}
