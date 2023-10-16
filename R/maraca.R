#' @description Creates the maraca analysis object as an S3 object of
#' class 'maraca'.
#'
#' @param data A data frame with columns for the following information:
#'             - outcome column, containing the time-to-event and continuous
#'               labels
#'             - arm column, containing the arm a given row belongs to.
#'             - value column, containing the values.
#' @param tte_outcomes A vector of strings containing the time-to-event
#'                     outcome labels. The order is kept for the plot.
#' @param continuous_outcome A single string containing the continuous
#'                           outcome label.
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
#' @param fixed_followup_days A mandatory specification of the integer number
#'                            of fixed follow-up days in the study.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#'
#' @return An object of class 'maraca'. The object information must be
#'         considered private.
#' @examples
#' data(hce_scenario_a)
#' hce_test <- maraca(
#'   data = hce_scenario_a,
#'   tte_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   continuous_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' @export
maraca <- function(
  data,
  tte_outcomes,
  continuous_outcome,
  arm_levels = c(
    active = "active",
    control = "control"
  ),
  column_names = c(
    outcome = "outcome",
    arm = "arm",
    value = "value"
  ),
  fixed_followup_days,
  compute_win_odds = FALSE
) {

  checkmate::assert_data_frame(data)
  checkmate::assert_character(tte_outcomes, any.missing = FALSE)
  checkmate::assert_string(continuous_outcome)
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

  checkmate::assert_int(fixed_followup_days)
  checkmate::assert_flag(compute_win_odds)

  `%>%` <- dplyr::`%>%`

  # Make sure that data is a data.frame
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Remove unwanted outcomes and arm levels, and normalise column names
  # in the internal data.
  # Note: We use HCE to refer to our internal, normalised data frame.
  # and with "data" to the user-provided, external, dirty data frame.
  hce_dat <- .reformat_and_check_data(data, tte_outcomes,
                                      continuous_outcome,
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
  if (fixed_followup_days <
        max(meta[meta$outcome %in% tte_outcomes, "maxday"])) {
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

  ecdf_by_outcome <- .compute_ecdf_by_outcome(
    hce_dat, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days
  )

  continuous <- .compute_continuous(
    hce_dat, meta, ecdf_by_outcome, tte_outcomes, continuous_outcome, arm_levels
  )

  win_odds <- list("win_odds" = NULL, "win_odds_outcome" = NULL)
  if (compute_win_odds) {
    win_odds <- .compute_win_odds(hce_dat, arm_levels)
  }

  return(
    structure(
      list(
        tte_outcomes = tte_outcomes,
        continuous_outcome = continuous_outcome,
        arm_levels = arm_levels,
        fixed_followup_days = fixed_followup_days,
        column_names = column_names,
        meta = meta,
        ecdf_by_outcome = ecdf_by_outcome,
        continuous = continuous,
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
#'   tte_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   continuous_outcome = "Continuous outcome",
#'   fixed_followup_days = 3 * 365,
#'   column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
#'   arm_levels = c(active = "Active", control = "Control"),
#'   compute_win_odds = TRUE
#' )
#' plot <- plot_maraca(hce_test)
#' @export
plot_maraca <- function(
    obj, continuous_grid_spacing_x = 10, trans = "identity",
    density_plot_type = "default",
    vline_type = "median",
    theme = "maraca") {
  checkmate::assert_class(obj, "maraca")
  checkmate::assert_int(continuous_grid_spacing_x)
  checkmate::assert_string(trans)
  checkmate::assert_choice(
    density_plot_type, c("default", "violin", "box", "scatter")
  )
  checkmate::assert_choice(
    vline_type, c("median", "mean", "none")
  )
  aes <- ggplot2::aes
  `%>%` <- dplyr::`%>%`

  meta <- obj$meta
  continuous <- obj$continuous
  ecdf_mod <- obj$ecdf_by_outcome
  win_odds <- obj$win_odds
  start_continuous_endpoint <-
    meta[meta$outcome == obj$continuous_outcome, ]$startx

  plotdata_ecdf <- ecdf_mod$data[, c("outcome", "arm",
                                     "adjusted.time", "ecdf_values")]
  plotdata_ecdf$type <- "tte"
  names(plotdata_ecdf) <- c("outcome", "arm", "x", "y", "type")
  plotdata_cont <- continuous$data[, c("outcome", "arm", "x", "y_level")]
  plotdata_cont$type <- "cont"
  names(plotdata_cont) <- c("outcome", "arm", "x", "y", "type")

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

  plotdata <- as.data.frame(rbind(plotdata_ecdf, plotdata_cont))

  scale <- sign(log10(continuous_grid_spacing_x)) * floor(
    abs(log10(continuous_grid_spacing_x))
  )

  minor_grid <- .minor_grid(
    continuous$data$value, scale, continuous_grid_spacing_x
  )

  zeroposition <- .to_rangeab(0,
    start_continuous_endpoint,
    min(continuous$data$value, na.rm = TRUE),
    max(continuous$data$value, na.rm = TRUE)
  )
  # Plot the information in the Maraca plot
  plot <- ggplot2::ggplot(plotdata) +
    ggplot2::geom_vline(
      xintercept = cumsum(c(0, meta$proportion)),
      color = "grey80"
    ) +
    ggplot2::geom_vline(
      xintercept = zeroposition,
      color = "white",
      linewidth = 1
    )

  if (vline_type == "median") {
    plot <- plot +
      ggplot2::geom_vline(
        mapping = aes(
          xintercept = median,
          color = arm
        ),
        data = continuous$meta,
        linetype = "dashed",
        linewidth = 0.8
      )
  } else if (vline_type == "mean") {
    plot <- plot +
      ggplot2::geom_vline(
        mapping = aes(
          xintercept = average,
          color = arm
        ),
        data = continuous$meta,
        linetype = "dashed",
        linewidth = 0.8
      )
  }

  plot <- plot +
    ggplot2::geom_step(
      data = plotdata[plotdata$type == "tte", ],
      aes(x = x, y = y, color = arm)
    )

  if (density_plot_type == "default") {
    plot <- plot +
      ggplot2::geom_violin(
        data = plotdata[plotdata$type == "cont", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      ) + ggplot2::geom_boxplot(
      data = plotdata[plotdata$type == "cont", ],
      aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5,
      width =
        abs(diff(as.numeric(unique(plotdata[plotdata$type == "cont", ]$y)))) /
        3
    )
  } else if (density_plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(
        data = plotdata[plotdata$type == "cont", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      )
  } else if (density_plot_type == "box") {
    plot <- plot +
      ggplot2::geom_boxplot(
        data = plotdata[plotdata$type == "cont", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      )
  } else if (density_plot_type == "scatter") {
    plot <- plot +
      ggplot2::geom_jitter(
        data = plotdata[plotdata$type == "cont", ],
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
      breaks = c(meta$proportion / 2 + meta$startx),
      labels = c(obj$tte_outcomes, obj$continuous_outcome),
      minor_breaks = .to_rangeab(
        minor_grid,
        start_continuous_endpoint,
        min(continuous$data$value, na.rm = TRUE),
        max(continuous$data$value, na.rm = TRUE)
      ),
      trans = trans
    ) +
    ggplot2::annotate(
      geom = "text",
      x = .to_rangeab(
        minor_grid,
        start_continuous_endpoint,
        min(continuous$data$value, na.rm = TRUE),
        max(continuous$data$value, na.rm = TRUE)
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
#'   tte_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   continuous_outcome = "Continuous outcome",
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

  `%>%` <- dplyr::`%>%`

  pb <- ggplot2::ggplot_build(x)
  plot_type <- class(as.list(x$layers[[5]])[["geom"]])[1]

  proportions <- diff(pb$data[[1]][, c("xintercept")])
  names(proportions) <- levels(x$data$outcome)

  arms <- levels(pb$plot$data[, pb$plot$labels$colour])

  tte_data <-
    utils::tail(utils::head(pb$data[[4]][, c("group", "x", "y")], -2), -2)
  tte_data$group <- factor(tte_data$group, labels = arms)

  scatter_data <- NULL
  boxstat_data <- NULL
  violin_data <- NULL

  if (plot_type == "GeomPoint") {
    scatter_data <- pb$data[[5]][, c("group", "x", "y")]
    scatter_data$group <- factor(scatter_data$group, labels = arms)
  } else if (plot_type == "GeomBoxplot") {
    boxstat_data <- pb$data[[5]] %>%
      dplyr::select(group, "x_lowest" = xmin_final,
                    "whisker_lower" = xmin,
                    "hinge_lower" = xlower, "median" = xmiddle,
                    "hinge_upper" = xupper, "whisker_upper" = xmax,
                    "x_highest" = xmax_final, outliers)
    boxstat_data$outliers <- lapply(boxstat_data$outliers, sort)
    boxstat_data$group <- factor(boxstat_data$group, labels = arms)
  } else if (plot_type == "GeomViolin") {
    violin_data <- pb$data[[5]][, c("group", "x", "y", "density", "width")]
    violin_data$group <- factor(violin_data$group, labels = arms)
    if (class(as.list(x$layers[[6]])[["geom"]])[1] == "GeomBoxplot") {
      plot_type <- paste(plot_type, "GeomBoxplot", sep = "+")
      boxstat_data <- pb$data[[6]] %>%
        dplyr::select(group, "x_lowest" = xmin_final,
                      "whisker_lower" = xmin,
                      "hinge_lower" = xlower, "median" = xmiddle,
                      "hinge_upper" = xupper, "whisker_upper" = xmax,
                      "x_highest" = xmax_final, outliers)
      boxstat_data$outliers <- lapply(boxstat_data$outliers, sort)
      boxstat_data$group <- factor(boxstat_data$group, labels = arms)
    }
  } else {
    stop(paste0("Unrecognised plot type ", plot_type))
  }

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
#'   tte_outcomes = c("Outcome I", "Outcome II", "Outcome III", "Outcome IV"),
#'   continuous_outcome = "Continuous outcome",
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
#' @param continuous_outcome A single string containing the continuous
#'                           outcome label. Default value "C".
#' @param arm_levels A named vector of exactly two strings, mapping the
#'                   values used for the active and control arms to the values
#'                   used in the data. The names must be "active" and "control"
#'                   in this order. Note that this parameter only need to
#'                   be specified if you have labels different from
#'                    "active" and "control".
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
#'                            Otherwise, this argument must be specified.
#'                            Note: If argument is specified and HCE object
#'                            contains PADY or TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @param theme Choose theme to style the plot. The default theme is "maraca".
#'        Options are "maraca", "maraca_old", "color1", "color2" and none".
#'        For more details, check the vignette called
#'        "Maraca Plots - Themes and Styling".
#'        [companion vignette for package users](themes.html)
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
plot.hce <- function(x, continuous_outcome = "C",
                     arm_levels = c(active = "A", control = "P"),
                     continuous_grid_spacing_x = 10,
                     trans = "identity",
                     density_plot_type = "default",
                     vline_type = "median",
                     fixed_followup_days = NULL,
                     compute_win_odds = FALSE,
                     theme = "maraca", ...) {

  checkmate::assert_int(continuous_grid_spacing_x)
  checkmate::assert_string(trans)
  checkmate::assert_choice(density_plot_type,
                           c("default", "violin", "box", "scatter"))
  checkmate::assert_choice(
    vline_type, c("median", "mean", "none")
  )

  maraca_obj <- .maraca_from_hce_data(x, continuous_outcome, arm_levels,
                                      fixed_followup_days,
                                      compute_win_odds)

  plot_maraca(maraca_obj, continuous_grid_spacing_x,
              trans, density_plot_type, vline_type, theme)
}
