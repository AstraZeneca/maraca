#' @description Creates the maraca analysis object as an S3 object of
#' class 'maraca'.
#'
#' @param data A data frame with columns for the following information:
#'             - outcome column, containing the time-to-event and continuous
#'               labels
#'             - arm column, containing the arm a given row belongs to.
#'             - value column, containing the values.
#' @param step_outcomes A vector of strings containing the outcome labels
#'                      for all outcomes displayed as part of the step function
#'                      on the left side of the plot.
#'                      The order is kept for the plot.
#' @param last_outcome A single string containing the last outcome label
#'                     displayed on the right side of the plot.
#' @param arm_levels A named vector of exactly two strings, mapping the
#'                   values used for the active and control arms to the values
#'                   used in the data. The names must be "active" and "control"
#'                   in this order. Note that this parameter only need to
#'                   be specified if you have labels different from
#'                    "active" and "control".
#' @param column_names A named vector to map the
#'        outcome, arm, value to the associated column names
#'        in the data. The vector names must match in order "outcome", "arm",
#'        and "value". Note that this parameter only need to be
#'        specified if you have column names different from the ones above.
#' @param fixed_followup_days A mandatory specification of the fixed follow-up
#'                            days in the study. Can be a single integer value
#'                            for all tte-outcomes or a vector with one
#'                            integer value per tte-outcome.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @param step_types The type of each outcome in the step_outcomes vector.
#'                   Can be a single string (if all outcomes of same type) or
#'                   a vector of same length as step_outcomes. Possible values
#'                   in the vector are "tte" (default) or "binary".
#' @param last_type A single string giving the type of the last outcome.
#'                  Possible values are "continuous" (default), "binary" or
#'                  "multinomial".
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param tte_outcomes Deprecated and substituted by the more general
#'                     'step_outcomes'. A vector of strings containing the
#'                     time-to-event outcome labels. The order is kept for the
#'                     plot.
#' @param continuous_outcome Deprecated and substituted by the more general
#'                           'last_outcome'. A single string containing the
#'                           continuous outcome label.
#' @return An object of class 'maraca'. The object information must be
#'         considered private.
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' @export
maraca <- function(
  data,
  step_outcomes,
  last_outcome,
  arm_levels = c(
    active = "active",
    control = "control"
  ),
  column_names = c(
    outcome = "outcome",
    arm = "arm",
    value = "value"
  ),
  fixed_followup_days = NULL,
  compute_win_odds = FALSE,
  step_types = "tte",
  last_type = "continuous",
  lowerBetter = FALSE,
  tte_outcomes = lifecycle::deprecated(),
  continuous_outcome = lifecycle::deprecated()
) {

  checkmate::assert_data_frame(data)

  if (lifecycle::is_present(tte_outcomes)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(tte_outcomes)",
                              "maraca(step_outcomes)")
    step_outcomes <- tte_outcomes
  }

  if (lifecycle::is_present(continuous_outcome)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(continuous_outcome)",
                              "maraca(last_outcome)")
    last_outcome <- continuous_outcome
  }

  checkmate::assert_character(step_outcomes, any.missing = FALSE)
  checkmate::assert_string(last_outcome)
  checkmate::assert_character(arm_levels, len = 2, any.missing = FALSE)
  checkmate::assert_names(
    names(arm_levels),
    permutation.of = c("active", "control")
  )
  checkmate::assert_character(column_names, len = 3, any.missing = FALSE)
  checkmate::assert_names(
    names(column_names),
    permutation.of = c("outcome", "arm", "value")
  )
  checkmate::assert_subset(column_names,
                           choices = names(data),
                           empty.ok = FALSE)

  checkmate::assert_numeric(fixed_followup_days)

  checkmate::assert_character(step_types)
  checkmate::assert_subset(step_types,
                           choices = c("tte", "binary"),
                           empty.ok = FALSE)

  if (!(length(step_types) %in% c(1, length(step_outcomes)))) {
    stop(paste("step_types needs to be either a single string or",
               "a vector with one value for each tte outcome"))
  }

  checkmate::assert_string(last_type)
  checkmate::assert_subset(last_type,
                           choices = c("continuous", "binary"),
                           empty.ok = FALSE)

  checkmate::assert_flag(lowerBetter)

  if (!(length(fixed_followup_days) %in%
          c(1, length(step_outcomes[step_types == "tte"])))) {
    stop(paste("fixed_followup_days needs to be either a single value or",
               "a vector with one value for each tte outcome"))
  }

  checkmate::assert_flag(compute_win_odds)

  `%>%` <- dplyr::`%>%`

  # Make sure that data is a data.frame
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Remove unwanted outcomes and arm levels, and normalise column names
  # in the internal data.
  # Note: We use HCE to refer to our internal, normalised data frame.
  # and with "data" to the user-provided, external, dirty data frame.
  hce_dat <- .reformat_and_check_data(data, step_outcomes,
                                      last_outcome,
                                      arm_levels, column_names)

  # Calculate meta information from the entire HCE dataset needed for plotting
  meta <- .compute_metainfo(hce_dat)

  # In the current implementation of the package,
  # the fixed follow-up days given cannot be smaller
  # than the follow-up times for all tte-outcomes in the
  # in the dataset - this has to do with the fact that
  # we don't have information on patients that had multiple
  # events of different severity - for example a patient
  # having a myocardial infarction on day 300 and dies day
  # 800 - if we now change follow-up time to 500, we will
  # discard the death event for this patient (after 500)
  # but will at the same time not include the MI since
  # we don't know about it
  if (any(fixed_followup_days <
            unlist(meta[meta$outcome %in% step_outcomes, "maxday"]))) {
    stop(paste("Time-to-event data contain events",
               "after the fixed_followup_days - either",
               "provide a longer follow-up time or",
               "re-derive your input dataset for the",
               "follow-up time provided."))
  }

  # Remove rows with missing values - previously done
  # automatically by survival package (should we done)
  # after the meta data is collected to keep information
  # on if missing data was removed
  hce_dat <- hce_dat %>%
    dplyr::filter(!is.na(value))

  # Vectorize step type if singular value
  if (length(step_types) == 1) {
    step_types <- rep(step_types, times = length(step_outcomes))
  }

  ecdf_by_outcome <- .compute_ecdf_by_outcome(
    hce_dat, meta, step_outcomes, step_types,
    last_outcome, arm_levels,
    fixed_followup_days
  )

  if (last_type == "continuous") {
    data_last_outcome <- .compute_continuous(
      hce_dat, meta, ecdf_by_outcome, step_outcomes, last_outcome, arm_levels
    )
  } else if (last_type == "binary") {
    data_last_outcome <- .compute_binary(
      hce_dat, meta, ecdf_by_outcome, step_outcomes, last_outcome, arm_levels
    )
  } else if (last_type == "multinomial") {
    data_last_outcome <- NULL
  }

  win_odds <- list("win_odds" = NULL, "win_odds_outcome" = NULL,
                   "wins_forest" = NULL, "wo_bar" = NULL)
  if (compute_win_odds) {
    win_odds <- .compute_win_odds(hce_dat, arm_levels,
                                  step_outcomes, last_outcome,
                                  lowerBetter)
  }

  return(
    structure(
      list(
        step_outcomes = step_outcomes,
        last_outcome = last_outcome,
        step_types = step_types,
        last_type = last_type,
        arm_levels = arm_levels,
        fixed_followup_days = fixed_followup_days,
        column_names = column_names,
        meta = meta,
        ecdf_by_outcome = ecdf_by_outcome,
        data_last_outcome = data_last_outcome,
        win_odds = win_odds[["win_odds"]],
        win_odds_outcome = win_odds[["win_odds_outcome"]],
        wins_forest = win_odds[["wins_forest"]],
        wo_bar = win_odds[["wo_bar"]],
        lowerBetter = lowerBetter
      ),
      class = c("maraca")
    )
  )
}

#' @param x an object of class maraca
#' @param ... further arguments passed to or
#' from other methods.
#' @method print maraca
#' @rdname maraca
#' @export
print.maraca <- function(x, ...) {

  `%>%` <- dplyr::`%>%`

  cat(paste("Maraca object for plotting maraca graph created for",
            sum(x$meta$n), "patients.\n\n"))

  if (sum(x$meta$missing) > 0) {
    cat(paste(sum(x$meta$missing),
              "patient(s) removed because of missing values.\n\n"))
  }

  if (!is.null(x$win_odds)) {
    cat(paste0("Win odds (95% CI): ", round(x$win_odds[[1]], 2),
               " (", round(x$win_odds[[2]], 2), ", ",
               round(x$win_odds[[3]], 2), ")", "\n",
               "Win odds p-value: ",
               format.pval(x$win_odds[[4]], digits = 3, eps = 0.001), "\n\n"))
  } else {
    cat("Win odds not calculated.\n\n")
  }

  tmp <- x$meta %>%
    dplyr::select(outcome, n, proportion,
                  dplyr::starts_with("n_"), missing) %>%
    as.data.frame()
  names(tmp) <- .title_case(gsub("_", " ", names(tmp)))
  print(tmp, row.names = FALSE)

}


#' Creates and returns the plot of the maraca data.
#'
#' @param obj an object of S3 class 'maraca'
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type which type of plot to display in the continuous
#'        part of the plot. Options are "default", "violin", "box", "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param remove_outliers Flag indicating for last endpoint if outliers are
#'        supposed to be displayed. If TRUE, the outliers are removed and
#'        only the range not including them is displayed. Only implemented
#'        for continuous endpoints. Default value FALSE.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#' @return a ggplot2 object of the data. This function
#' will not render the plot immediately. You have to print() the returned
#' object for it to be displayed.
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' plot <- plot_maraca(hce_test)
#' @export
plot_maraca <- function(
    obj, continuous_grid_spacing_x = NULL,
    trans = c("identity", "log", "log10", "sqrt", "reverse")[1],
    density_plot_type = c("default", "violin", "box", "scatter")[1],
    vline_type = NULL,
    remove_outliers = FALSE,
    theme = "maraca") {

  .run_plot_checks(obj, continuous_grid_spacing_x, remove_outliers, trans)

  aes <- ggplot2::aes

  meta <- obj$meta
  step_outcomes <- obj$step_outcomes
  step_types <- obj$step_types
  which_tte <- which(step_types == "tte")
  which_binary <- which(step_types == "binary")
  last_data <- obj$data_last_outcome
  last_meta <- last_data$meta
  last_type <- obj$last_type

  vline_type <- .assign_vline_type(last_type, density_plot_type, vline_type)

  win_odds <- obj$win_odds
  start_last_endpoint <-
    meta[meta$outcome == obj$last_outcome, ]$startx

  if (is.null(continuous_grid_spacing_x)) {
    continuous_grid_spacing_x <- 10
  }
  scale <- sign(log10(continuous_grid_spacing_x)) * floor(
    abs(log10(continuous_grid_spacing_x))
  )

  plotdata_ecdf <- .prepare_ecdf_plot_data(obj, step_outcomes)

  plotdata_last <- last_data$data[, c("outcome", "arm", "value", "x", "y")]
  plotdata_last$type <- last_type

  if (last_type == "continuous") {

    res <- .prepare_continuous_plot_data(plotdata_last, last_meta, trans,
                                         density_plot_type,
                                         remove_outliers, start_last_endpoint)

    plotdata_last <- res$plotdata_last
    boxplot_data <- res$boxplot_data
    violin_scaling_factor <- res$violin_scaling_factor
    last_meta <- res$last_meta

  }

  grid <- .create_grid(plotdata_last, last_type, trans, last_data,
                       scale, continuous_grid_spacing_x)
  range <- grid$range
  minor_grid <- grid$minor_grid
  minor_grid_x <- grid$minor_grid_x

  vline_data <- .prepare_vline_data(last_meta, vline_type, trans)

  if (trans == "reverse") {
    if (!is.null(win_odds) && !obj$lowerBetter) {
      message(paste("Last endpoint axis has been reversed, which might",
                    "indicate that lower values are considered advantageous.",
                    "Note that the win odds were calculated assuming that",
                    "higher values are better. If that is not correct, please",
                    "use the parameter lowerBetter = TRUE in the",
                    "maraca function."))
    }

    minor_grid_x <- rev(minor_grid_x)
    minor_grid <- rev(minor_grid)
    plotdata_last$x <- start_last_endpoint - plotdata_last$x + 100

    if (last_type == "continuous" &&
          density_plot_type %in% c("default", "box")) {
      boxplot_data$stats <- .reverse_boxplot_stats(boxplot_data$stats,
                                                   start_last_endpoint)
    }

    if (!is.null(vline_data)) {
      vline_data$x <- start_last_endpoint - vline_data$x + 100
    }
  }

  plotdata <- rbind(plotdata_ecdf, plotdata_last)

  # Plot the information in the Maraca plot
  plot <- .set_up_initial_plot(plotdata, meta, vline_data)

  for (outcome in step_outcomes[which_tte]) {
    plot <- plot +
      ggplot2::geom_step(data =
                           plotdata[plotdata$outcome == outcome, ],
                         aes(x = x, y = y, color = arm))
  }

  if (length(which_binary) > 0) {
    plot <- .add_binary_steps_to_plot(plot, plotdata, step_outcomes,
                                      step_types, which_binary)
  }

  if (step_types[length(step_types)] == "binary") {
    plot <- .add_end_binary_step(plot, plotdata, step_outcomes)
  }

  width <- diff(range(plotdata_last$y))

  if (density_plot_type == "default") {
    if (last_type == "continuous") {
      plot <- plot +
        ggplot2::geom_polygon(mapping = aes(x, y, group = arm,
                                            fill = arm, colour = arm),
                              data = plotdata[plotdata$type == "violin", ],
                              alpha = 0.5,
                              show.legend = FALSE)
      plot <- .add_boxplot(plot, boxplot_data, (width / 3),
                           add_v_lines = FALSE)
    } else if (last_type == "binary") {
      plot <- plot +
        ggplot2::geom_polygon(data = plotdata[plotdata$type == last_type, ],
                              ggplot2::aes(x = x, y = y, color = arm,
                                           fill = arm),
                              alpha = 0.5,
                              show.legend = FALSE) +
        ggplot2::geom_point(data = last_meta,
                            ggplot2::aes(x = average, y = y,
                                         color = arm))
    }
  } else if (density_plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_polygon(mapping = aes(x, y, group = arm,
                                          fill = arm, colour = arm),
                            data = plotdata[plotdata$type == "violin", ],
                            alpha = 0.5)
  } else if (density_plot_type == "box") {
    plot <- .add_boxplot(plot, boxplot_data, (0.75 * width),
                         add_v_lines = TRUE)
  } else if (density_plot_type == "scatter") {
    plot <- plot +
      ggplot2::geom_jitter(
        data = plotdata[plotdata$type == last_type, ],
        aes(x = x, y = y, color = arm),
        # Jittering only vertically, keep the correct x-value
        width = 0
      )
  }

  plot <- .add_labels_to_plot(plot, minor_grid, minor_grid_x, scale, range,
                              start_last_endpoint, trans, obj, meta)

  attr(plot, "density_type") <- density_plot_type

  if (!is.null(win_odds)) {

    plot <- .add_win_odds_to_plot(plot, win_odds, 0, Inf,
                                  hjust = 0)

    # Meta data on win odds will be added to plot
    win_odds <- unname(win_odds)
    params <- list(
      "win_odds" = win_odds[[1]],
      "lower_ci" = win_odds[[2]],
      "upper_ci" = win_odds[[3]],
      "p_value" = win_odds[[4]]
    )

    # Add win odds meta data as a label so retrievable
    attr(plot, "win.odds") <- params
  }
  if (last_type == "continuous" &&
        (density_plot_type %in% c("default", "violin"))) {
    attr(plot, "violin_scaling_factor") <- violin_scaling_factor
  }

  plot <- .add_theme_to_plot(plot, theme)

  # Add label to plot - maracaPlot
  class(plot) <- c("maracaPlot", class(plot))

  return(plot)
}

#' Generic function to generate validation data for the maraca plot object.
#'
#' This will produce the 4 validation datasets.
#'
#' @param x An object of S3 class 'maracaPlot'.
#' @param \dots Not used.
#' @return Creates a list of datasets for validation purposes.
#'
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' p <- plot(hce_test)
#' validate_maraca_plot(p)
#'
#' @export
validate_maraca_plot <- function(x,  ...) {
  checkmate::assert_class(x, "maracaPlot")

  pb <- ggplot2::ggplot_build(x)
  layers <- sapply(pb$plot$layers, function(lb) {
    class(lb$geom)[1]
  })

  proportions <- diff(pb$data[[1]][, c("xintercept")])
  names(proportions) <- unique(x$data$outcome)

  arms <- levels(unlist(pb$plot$data[, pb$plot$labels$colour]))

  tte_data <- .create_validation_tte(layers, x, arms)
  binary_step_data <- .create_validation_binary_step(layers, x, arms)
  binary_last_data <- .create_validation_binary_last(layers, x, arms)
  scatter_data <- .create_validation_scatter(layers, x, arms)
  boxstat_data <- .create_validation_box(layers, x, arms)
  violin_data <- .create_validation_violin(layers, x, arms)

  plot_type <- attr(x, "density_type")

  if ("win.odds" %in% names(attributes(x))) {
    params <- attr(x, "win.odds")
    wo_stats <- c(winodds = params$win_odds,
                  lowerCI = params$lower_ci,
                  upperCI = params$upper_ci,
                  p_value = params$p_value)
  } else {
    wo_stats <- NULL
  }

  return(
    list(
      plot_type = plot_type,
      proportions = proportions,
      tte_data = tte_data,
      binary_step_data = binary_step_data,
      binary_last_data = binary_last_data,
      scatter_data = scatter_data,
      boxstat_data = boxstat_data,
      violin_data = violin_data,
      wo_stats = wo_stats
    )
  )
}

#' Generic function to plot the maraca object using plot().
#'
#' @param x An object of S3 class 'maraca'.
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param remove_outliers Flag indicating for last endpoint if outliers are
#'        supposed to be displayed. If TRUE, the outliers are removed and
#'        only the range not including them is displayed. Only implemented
#'        for continuous endpoints. Default value FALSE.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#' @param \dots not used
#' @return Returns ggplot2 plot of the maraca object.
#'
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   step_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   last_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' plot(hce_test)
#'
#' @export
plot.maraca <- function(
    x,
    continuous_grid_spacing_x = 10,
    trans = c("identity", "log", "log10", "sqrt", "reverse")[1],
    density_plot_type = c("default", "violin", "box", "scatter")[1],
    vline_type = NULL,
    remove_outliers = FALSE,
    theme = "maraca",
    ...) {
  plot_maraca(x, continuous_grid_spacing_x,
              trans, density_plot_type,
              vline_type, remove_outliers, theme)
}
#' Generic function to plot the adhce object using plot().
#'
#' @param x an object of S3 class 'adhce'.
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
#'                   "active" and "control".
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the x-axis scale for the last
#'        outcome. Possible values are "identity", "log" (only for continuous
#'        endpoint), "log10" (only for continuous endpoint), "sqrt" (only for
#'        continuous endpoint) and "reverse". The default value is "identity".
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median" (only for continuous last endpoint), "mean", "none" and
#'        NULL (default). By default (vline_type = NULL), vline_type will be
#'        set to "median" for a continuous last endpoint and to "mean" for
#'        a binary last endpoint.
#' @param remove_outliers Flag indicating for last endpoint if outliers are
#'        supposed to be displayed. If TRUE, the outliers are removed and
#'        only the range not including them is displayed. Only implemented
#'        for continuous endpoints. Default value FALSE.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @param step_types The type of each outcome in the step_outcomes vector.
#'                   Can be a single string (if all outcomes of same type) or
#'                   a vector of same length as step_outcomes. Possible values
#'                   in the vector are "tte" (default) or "binary".
#' @param last_type A single string giving the type of the last outcome.
#'                  Possible values are "continuous" (default), "binary" or
#'                  "multinomial".
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#'        [companion vignette for package users](themes.html)
#' @param lowerBetter Flag for the final outcome variable, indicating if
#'                    lower values are considered better/advantageous.
#'                    This flag is need to make sure the win odds are
#'                    calculated correctly.
#'                    Default value is FALSE, meaning higher values
#'                    are considered advantageous.
#' @param tte_outcomes Deprecated and substituted by the more general
#'                     'step_outcomes'. A vector of strings containing the
#'                     time-to-event outcome labels. The order is kept for the
#'                     plot.
#' @param continuous_outcome Deprecated and substituted by the more general
#'                           'last_outcome'. A single string containing the
#'                           continuous outcome label.
#' @param \dots not used
#' @return Returns ggplot2 plot of the hce object.
#'
#' @examples
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' hce_dat <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
#'              seed = 31337)
#' plot(hce_dat)
#' plot(hce_dat, fixed_followup_days = 3 * 365)
#'
#' @export
plot.adhce <- function(x,
                       step_outcomes = NULL,
                       last_outcome = "C",
                       arm_levels = c(active = "A", control = "P"),
                       continuous_grid_spacing_x = 10,
                       trans = c("identity", "log", "log10",
                                 "sqrt", "reverse")[1],
                       density_plot_type = c("default", "violin",
                                             "box", "scatter")[1],
                       vline_type = NULL,
                       remove_outliers = FALSE,
                       compute_win_odds = FALSE,
                       step_types = "tte",
                       last_type = "continuous",
                       theme = "maraca",
                       lowerBetter = FALSE,
                       tte_outcomes = lifecycle::deprecated(),
                       continuous_outcome = lifecycle::deprecated(),
                       ...) {

  if (lifecycle::is_present(tte_outcomes)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(tte_outcomes)",
                              "maraca(step_outcomes)")
    step_outcomes <- tte_outcomes
  }

  if (lifecycle::is_present(continuous_outcome)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(continuous_outcome)",
                              "maraca(last_outcome)")
    last_outcome <- continuous_outcome
  }

  maraca_obj <- .maraca_from_hce_data(x, step_outcomes,
                                      last_outcome, arm_levels,
                                      compute_win_odds = compute_win_odds,
                                      step_types = step_types,
                                      last_type = last_type,
                                      lowerBetter = lowerBetter)

  plot_maraca(maraca_obj, continuous_grid_spacing_x,
              trans, density_plot_type, vline_type, remove_outliers,
              theme)
}
