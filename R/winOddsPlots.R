
#' @export
component_plot <- function(x, ...) {
  UseMethod("component_plot", x)
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome directly from an hce object.
#' Check the vignette "Plotting win odds" for more details.
#'
#' @param x an object of S3 class 'hce'.
#' @param \dots not used
#' @param continuous_outcome A single string containing the continuous
#'                           outcome label. Default value "C".
#' @param fixed_followup_days Not needed if HCE object contains information
#'                            on fixed follow-up days in the study
#'                            (column PADY or TTEfixed depending on hce
#'                            version). Otherwise,
#'                            this argument must be specified.
#'                            Note: If argument is specified and HCE object
#'                            contains PADY/TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param theme Different themes to style the graph. Default style is
#'              "maraca".
#' @return Component plot as a ggplot2 object.
#' @examples
#' set.seed(31337)
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3)
#' component_plot.hce(hce_dat)
#'
#' @export
component_plot.hce <- function(x, continuous_outcome = "C",
                               fixed_followup_days = NULL,
                               theme = "maraca",
                               ...) {

  # Checks
  checkmate::assert_string(continuous_outcome)
  checkmate::assertNames(names(x), must.include = "GROUP")

  x <- as.data.frame(x)
  # Helper variable - any outcome not declared as continuous will
  # be included as time-to-event outcome
  tte <- sort(unique(x$GROUP)[unique(x$GROUP) != continuous_outcome])

  # Make sure that name change in variable TTEFixed in newer
  # version of HCE package is working as well
  if ("PADY" %in% names(x)) {
    x$TTEfixed <- x$PADY
  }

  # Fixed follow-up days - if not provided use information
  # from HCE object
  if (is.null(fixed_followup_days)) {
    checkmate::assertNames(names(x), must.include = "TTEfixed")
    checkmate::assert_int(x$TTEfixed[[1]])
    fixed_followup_days <- x$TTEfixed[[1]]
  }

  # Create maraca object
  maraca_dat <- maraca::maraca(
    data = x,
    tte_outcomes = tte,
    continuous_outcome = continuous_outcome,
    column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
    arm_levels = c(active = "A", control = "P"),
    fixed_followup_days = fixed_followup_days,
    compute_win_odds = TRUE
  )

  # Get win odds by outcome from maraca object
  win_odds_outcome <- maraca_dat$win_odds_outcome
  # List of outcomes in order of plotting
  endpoints <- c(maraca_dat$tte_outcomes, maraca_dat$continuous_outcome)
  # Create data set for potting
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints)
  # Create component plot
  plot <- .create_component_plot(wo_bar_nc, endpoints)

  plot <- plot +
    ggplot2::geom_vline(xintercept = seq(0.5, length(endpoints) + 1.5, 1),
                        linetype = 2, linewidth = 0.5, color = "darkgray") +
    # Axis showing percentages
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x, 2), "%")) +
    ggplot2::ylab("Percent of all comparisons") +
    ggplot2::scale_fill_manual(values = c("#F8766D", "#00BFC4",
                                          "#d3d3d3"), name = NULL)  +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  return(plot)
}

#' Generic function to create a plot showing the components used in
#' calculating win odds (wins and ties) separately for each
#' outcome directly from a maraca object.
#' Note that for this plot, when creating the maraca object using the maraca()
#' function, the argument "compute_win_odds" has to be set to TRUE.
#' Check the vignette "Plotting win odds" for more details.
#'
#' @param x an object of S3 class 'maraca'.
#' @param \dots not used
#' @param theme Different themes to style the graph. Default style is
#'              "maraca".
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
#' component_plot.hce(maraca_dat)
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
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints)
  # Create component plot
  plot <- .create_component_plot(wo_bar_nc, endpoints)

  plot <- plot +
    ggplot2::geom_vline(xintercept = seq(0.5, length(endpoints) + 1.5, 1),
                        linetype = 2, linewidth = 0.5, color = "darkgray") +
    # Axis showing percentages
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x, 2), "%")) +
    ggplot2::ylab("Percent of all comparisons") +
    ggplot2::scale_fill_manual(values = c("#F8766D", "#00BFC4",
                                          "#d3d3d3"), name = NULL)  +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())

  return(plot)
}


##' @export
component_plot.default <- function(x,
                                   ...) {
  paste0("component_plot() function can only handle inputs of class ",
         "'hce' or 'maraca'. Your input has class ", class(x), ".")
}


# Preparing dataset to be used for plotting components
.prep_data_component_plot <- function(win_odds_outcome, endpoints) {

  `%>%` <- dplyr::`%>%`

  # Win odds summary for each outcome from maraca object
  wo_bar_nc <- win_odds_outcome$summary_by_GROUP

  # Add overall numbers
  wo_tot <- win_odds_outcome$summary
  wo_tot <- wo_tot %>%
    dplyr::mutate("GROUP" = "Overall") %>%
    dplyr::select(names(win_odds_outcome$summary_by_GROUP))

  wo_bar_nc <- rbind(wo_tot, wo_bar_nc)

  wo_bar_nc <- wo_bar_nc %>%
    # Order according to outcome
    dplyr::arrange(match(GROUP, endpoints)) %>%
    # Wide format to get 1 line per outcome
    tidyr::pivot_wider(names_from = TRTP,
                       values_from = c(WIN, LOSS, TIE, TOTAL)) %>%
    # Selecting variables of interest and renaming for plotting
    dplyr::select(GROUP, "Active_wins" = WIN_A, "Placebo_wins" = WIN_P,
                  "Ties" = TIE_A) %>%
    # Long format for plotting
    tidyr::pivot_longer(cols = c("Active_wins", "Placebo_wins", "Ties"),
                        names_to = "name", values_to = "value")

  # Total number of wins/losses/ties to get relative results
  wo_bar_nc$total <- wo_tot$TOTAL[1]

  # Calculate percentage results
  wo_bar_nc$percentage <- 100 * (wo_bar_nc$value / wo_bar_nc$total)

  return(wo_bar_nc)
}


# The main plotting function creating the component plot
.create_component_plot <- function(wo_bar_nc, endpoints) {

  aes <- ggplot2::aes

  wo_bar_nc$GROUP <- factor(wo_bar_nc$GROUP,
                            levels = rev(c("Overall", endpoints)))

  plot <-
    ggplot2::ggplot(data = wo_bar_nc, aes(x = GROUP, y = percentage,
                                          fill = name)) +
    # Bars
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      width = .8) +
    # Flip to show bars horizontally
    ggplot2::coord_flip() +
    # Add wins/losses/ties as labels
    ggplot2::geom_text(aes(label = round(percentage, 1)),
                       position = ggplot2::position_dodge(width = .8),
                       vjust = 0.5, hjust = "inward")

  return(plot)
}
