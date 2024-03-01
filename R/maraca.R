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

  win_odds <- list("win_odds" = NULL, "win_odds_outcome" = NULL)
  if (compute_win_odds) {
    win_odds <- .compute_win_odds(hce_dat, arm_levels)
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
        win_odds_outcome = win_odds[["win_odds_outcome"]]
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
#' @param trans the transformation to apply to the data before plotting.
#'        The accepted values are the same that ggplot2::scale_x_continuous
#' @param density_plot_type which type of plot to display in the continuous
#'        part of the plot. Options are "default", "violin", "box", "scatter".
#' @param vline_type what the vertical lines in the continuous part of the plot
#'        should highlight. Options are "median", "mean", "none".
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
    trans = "identity",
    density_plot_type = "default",
    vline_type = NULL,
    theme = "maraca") {

  checkmate::assert_class(obj, "maraca")

  if (!(is.null(continuous_grid_spacing_x) ||
          is.numeric(continuous_grid_spacing_x))) {
    stop("continuous_grid_spacing_x has to be numeric or NULL")
  }

  checkmate::assert_string(trans)

  aes <- ggplot2::aes
  `%>%` <- dplyr::`%>%`

  meta <- obj$meta
  step_outcomes <- obj$step_outcomes
  step_types <- obj$step_types
  which_tte <- which(step_types == "tte")
  which_binary <- which(step_types == "binary")
  last_data <- obj$data_last_outcome
  last_type <- obj$last_type

  vline_type <-
    switch(last_type,
           "continuous" = .checks_continuous_outcome(density_plot_type,
                                                     vline_type),
           "binary" = .checks_binary_outcome(density_plot_type,
                                             vline_type),
           stop("Unsupported last outcome type"))

  ecdf_mod <- obj$ecdf_by_outcome
  win_odds <- obj$win_odds
  start_last_endpoint <-
    meta[meta$outcome == obj$last_outcome, ]$startx

  if (is.null(continuous_grid_spacing_x)) {
    continuous_grid_spacing_x <- ifelse(last_type == "continuous", 10, 0.1)
  }

  plotdata_ecdf <- ecdf_mod$data[, c("outcome", "arm",
                                     "adjusted.time", "step_values",
                                     "type")]
  names(plotdata_ecdf) <- c("outcome", "arm", "x", "y", "type")
  plotdata_last <- last_data$data[, c("outcome", "arm", "x", "y")]
  plotdata_last$type <- last_type
  names(plotdata_last) <- c("outcome", "arm", "x", "y", "type")

  # Add points at (0, 0) on both curves so that they start from the origin
  add_points <- plotdata_ecdf %>%
    dplyr::group_by(arm) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup()

  add_points$x <- 0
  add_points$y <- 0
  plotdata_ecdf <- rbind(
    add_points,
    plotdata_ecdf
  )

  plotdata_ecdf <- plotdata_ecdf[order(plotdata_ecdf$x), ]

  # Add end point of previous curve to avoid jumps
  add_points <-
    do.call("rbind",
            lapply(2:length(step_outcomes),
                   function(i) {
                     plotdata_ecdf %>%
                       dplyr::group_by(arm) %>%
                       dplyr::filter(outcome == step_outcomes[i - 1]) %>%
                       dplyr::slice_tail(n = 1) %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(outcome = step_outcomes[i]) %>%
                       dplyr::ungroup()
                   }))

  plotdata_ecdf <- rbind(
    add_points,
    plotdata_ecdf
  )
  plotdata_ecdf <- plotdata_ecdf[order(plotdata_ecdf$x), ]

  # Add points at (100, y) on both curves so that they end at x=100%
  add_points <- plotdata_ecdf %>%
    dplyr::group_by(arm) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  add_points$x <- 100
  plotdata_ecdf <- rbind(
    plotdata_ecdf,
    add_points
  )

  plotdata_ecdf <- plotdata_ecdf[order(plotdata_ecdf$x), ]

  plotdata <- as.data.frame(rbind(plotdata_ecdf, plotdata_last))

  scale <- sign(log10(continuous_grid_spacing_x)) * floor(
    abs(log10(continuous_grid_spacing_x))
  )

  if (last_type == "continuous") {

    minor_grid <- .minor_grid(
      last_data$data$value, scale, continuous_grid_spacing_x
    )

    range <- c(min(last_data$data$value, na.rm = TRUE),
               max(last_data$data$value, na.rm = TRUE))

  } else if (last_type == "binary") {

    minor_grid <- seq(0, 1, continuous_grid_spacing_x)
    range <- c(0, 1)

  }

  # Plot the information in the Maraca plot
  plot <- ggplot2::ggplot(plotdata) +
    ggplot2::geom_vline(
      xintercept = cumsum(c(0, meta$proportion)),
      color = "grey80"
    )

  if (vline_type == "median") {
    plot <- plot +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = median,
          color = arm
        ),
        data = last_data$meta,
        linetype = "dashed",
        linewidth = 0.8,
        show.legend = FALSE
      )
  } else if (vline_type == "mean") {
    plot <- plot +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = average,
          color = arm
        ),
        data = last_data$meta,
        linetype = "dashed",
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  for (outcome in step_outcomes[which_tte]) {
    plot <- plot +
      ggplot2::geom_step(data = plotdata[plotdata$outcome == outcome, ],
                         aes(x = x, y = y, color = arm))
  }

  if (length(which_binary) > 0) {

    tmp <- plotdata[plotdata$outcome %in% step_outcomes[which_binary], ]

    tmp <- tmp[order(tmp$x), ]

    if (step_types[length(step_types)] == "binary") {
      tmp <- dplyr::slice_head(tmp, n = -2)
    }

    tmp1 <- tmp %>%
      dplyr::group_by(outcome, arm) %>%
      dplyr::summarize("xend" = max(x),
                       "x" = min(x),
                       "y" = min(y)) %>%
      dplyr::ungroup()

    tmp2 <- tmp %>%
      dplyr::group_by(outcome, arm) %>%
      dplyr::summarize("x" = max(x),
                       "yend" = max(y),
                       "y" = min(y)) %>%
      dplyr::ungroup()

    plot <- plot +
      ggplot2::geom_segment(
        data = tmp1,
        aes(x = x, y = y, xend = xend, yend = y,
            color = arm)
      ) +
      ggplot2::geom_segment(
        data = tmp2,
        aes(x = x, y = y, xend = x, yend = yend,
            group = arm),
        color = "darkgrey", linetype = 2
      )
  }

  if (step_types[length(step_types)] == "binary") {

    tmp <- plotdata %>%
      dplyr::filter(outcome == step_outcomes[length(step_types)]) %>%
      dplyr::group_by(arm) %>%
      dplyr::slice_tail(n = -1) %>%
      dplyr::summarize("xend" = max(x),
                       "x" = min(x),
                       "y" = max(y)) %>%
      dplyr::ungroup()

    plot <- plot +
      ggplot2::geom_segment(
        data = tmp,
        aes(x = x, y = y, xend = xend, yend = y,
            color = arm)
      )
  }

  if (density_plot_type == "default") {
    if (last_type == "continuous") {
      plot <- plot +
        ggplot2::geom_violin(
          data = plotdata_last,
          aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
        ) + ggplot2::geom_boxplot(
        data = plotdata_last,
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5,
        width =
          abs(diff(as.numeric(unique(plotdata_last$y)))) / 3
      )
    } else if (last_type == "binary") {
      plot <- plot +
        ggplot2::geom_polygon(data = plotdata_last,
                              ggplot2::aes(x = x, y = y, color = arm,
                                           fill = arm),
                              alpha = 0.5,
                              show.legend = FALSE) +
        ggplot2::geom_point(data = last_data$meta,
                            ggplot2::aes(x = average, y = y,
                                         color = arm))
    }
  } else if (density_plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(
        data = plotdata[plotdata$type == "continuous", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      )
  } else if (density_plot_type == "box") {
    plot <- plot +
      ggplot2::geom_boxplot(
        data = plotdata[plotdata$type == "continuous", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      )
  } else if (density_plot_type == "scatter") {
    plot <- plot +
      ggplot2::geom_jitter(
        data = plotdata[plotdata$type == "continuous", ],
        aes(x = x, y = y, color = arm),
        # Jittering only vertically, keep the correct x-value
        width = 0
      )
  }

  labels <- lapply(
    minor_grid,
    function(x) {
      s <- ifelse(scale > 0, 0, scale)
      return(as.character(round(x, -s + 1)))
    }
  )
  plot <- plot +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(meta$proportion / 2 + meta$startx + 0.1),
      labels = c(obj$step_outcomes, obj$last_outcome),
      minor_breaks = .to_rangeab(
        minor_grid,
        start_last_endpoint,
        range[1],
        range[2]
      ),
      trans = trans
    ) +
    ggplot2::annotate(
      geom = "text",
      x = .to_rangeab(
        minor_grid,
        start_last_endpoint,
        range[1],
        range[2]
      ),
      y = 0,
      label = labels,
      color = "grey60"
    )

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
    plot$labels$win.odds <- params
  }

  plot <- switch(theme,
                 "maraca" = .theme_maraca(plot),
                 "maraca_old" = .theme_maraca_old(plot),
                 "color1" = .theme_color1(plot),
                 "color2" = .theme_color2(plot),
                 "none" = plot,
                 stop("Please provide theme that exists"))

  plot <- plot +
    ggplot2::theme(
      axis.ticks.x.bottom = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

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

  arms <- levels(pb$plot$data[, pb$plot$labels$colour])

  tte_data <- .create_validation_tte(layers, x, arms)
  binary_step_data <- .create_validation_binary_step(layers, x, arms)
  binary_last_data <- .create_validation_binary_last(layers, x, arms)
  scatter_data <- .create_validation_scatter(layers, x, arms)
  boxstat_data <- .create_validation_box(layers, x, arms)
  violin_data <- .create_validation_violin(layers, x, arms)

  possible_plot_types <- c("GeomViolin", "GeomBoxplot", "GeomPoint")
  plot_type <- paste(possible_plot_types[possible_plot_types %in% layers],
                     collapse = "+")

  if ("win.odds" %in% names(x$labels)) {
    params <- x$labels$win.odds
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
#' @param \dots not used
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the data before plotting.
#'        The accepted values are the same that ggplot2::scale_x_continuous
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median", "mean", "none".
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#' @return Used for side effect. Returns ggplot2 plot of the maraca object.
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
    x, continuous_grid_spacing_x = 10, trans = "identity",
    density_plot_type = "default",
    vline_type = "median",
    theme = "maraca",
    ...) {
  plot_maraca(x, continuous_grid_spacing_x,
              trans, density_plot_type,
              vline_type, theme)
}
#' Generic function to plot the hce object using plot().
#'
#' @param x an object of S3 class 'hce'.
#' @param \dots not used
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
#' @param trans the transformation to apply to the data before plotting.
#'        The accepted values are the same that ggplot2::scale_x_continuous
#' @param density_plot_type The type of plot to use to represent the density.
#'        Accepts "default", "violin", "box" and "scatter".
#' @param vline_type what the vertical dashed line should represent. Accepts
#'        "median", "mean", "none".
#' @param fixed_followup_days Not needed if HCE object contains information
#'                            on fixed follow-up days in the study
#'                            (column PADY or TTEfixed,
#'                            depending on hce version).
#'                            Otherwise, this argument must be specified
#'                            to give the fixed follow-up days in the study.
#'                            Can be a single integer value
#'                            for all tte-outcomes or a vector with one
#'                            integer value per tte-outcome.
#'                            Note: If argument is specified and HCE object
#'                            also contains PADY or TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @param last_type A single string giving the type of the last outcome.
#'                  Possible values are "continuous" (default), "binary" or
#'                  "multinomial".
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#'        [companion vignette for package users](themes.html)
#' @param continuous_outcome Deprecated and substituted by the more general
#'                           'last_outcome'. A single string containing the
#'                           continuous outcome label.
#' @return Used for side effect. Returns ggplot2 plot of the hce object.
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
plot.hce <- function(x, last_outcome = "C",
                     arm_levels = c(active = "A", control = "P"),
                     continuous_grid_spacing_x = 10,
                     trans = "identity",
                     density_plot_type = "default",
                     vline_type = "median",
                     fixed_followup_days = NULL,
                     compute_win_odds = FALSE,
                     step_types = "tte",
                     last_type = "continuous",
                     theme = "maraca",
                     continuous_outcome = lifecycle::deprecated(),
                     ...) {

  if (lifecycle::is_present(continuous_outcome)) {
    lifecycle::deprecate_warn("0.7.0", "maraca(continuous_outcome)",
                              "maraca(last_outcome)")
    last_outcome <- continuous_outcome
  }

  maraca_obj <- .maraca_from_hce_data(x, last_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds,
                                      step_types = step_types,
                                      last_type = last_type)

  plot_maraca(maraca_obj, continuous_grid_spacing_x,
              trans, density_plot_type, vline_type, theme)
}
