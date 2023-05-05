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
    identical.to = c("active", "control")
  )
  checkmate::assert_character(column_names, len = 3, any.missing = FALSE)
  checkmate::assert_names(
    names(column_names),
    identical.to = c("outcome", "arm", "value")
  )
  checkmate::assert_int(fixed_followup_days)
  checkmate::assert_flag(compute_win_odds)

  `%>%` <- dplyr::`%>%`

  # Remove unwanted outcomes and arm levels, and normalise column names
  # in the internal data.
  # Note: We use HCE to refer to our internal, normalised data frame.
  # and with "data" to the user-provided, external, dirty data frame.
  HCE <- .reformat_and_check_data(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names)

  # Calculate meta information from the entire HCE dataset needed for plotting
  meta <- .compute_metainfo(HCE)

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
  HCE <- HCE %>%
    dplyr::filter(!is.na(value))

  ecdf_by_outcome <- .compute_ecdf_by_outcome(
    HCE, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days
  )

  continuous <- .compute_continuous(
    HCE, meta, ecdf_by_outcome, tte_outcomes, continuous_outcome, arm_levels
  )

  win_odds <- NULL
  if (compute_win_odds) {
    win_odds <- .compute_win_odds(HCE)
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
        win_odds = win_odds
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

  tmp <- as.data.frame(x$meta[, c("outcome", "n", "proportion",
                                  "n_active", "n_control", "missing")])
  names(tmp) <- toupper(names(tmp))
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
    vline_type = "median") {
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
  start_continuous_endpoint <- meta[
    meta$outcome == obj$continuous_outcome, ]$startx

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
      size = 1
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
        size = 0.8
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
        size = 0.8
      )
  }

  plot <- plot +
    ggplot2::geom_step(
    data = plotdata[plotdata$type == "tte", ],
      aes(x = x, y = y, color = arm)
    )

  plot <- plot +
    ggplot2::scale_color_discrete("Arm", labels = obj$arm_levels)

  if (density_plot_type == "default") {
    plot <- plot +
      ggplot2::geom_violin(
        data = plotdata[plotdata$type == "cont", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5
      ) + ggplot2::geom_boxplot(
        data = plotdata[plotdata$type == "cont", ],
        aes(x = x, y = y, colour = arm, fill = arm), alpha = 0.5,
        width = abs(diff(as.numeric(unique(
          plotdata[plotdata$type == "cont", ]$y)))) / 3
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
    ggplot2::xlab("Type of endpoint") +
    ggplot2::ylab("Cumulative proportion") +
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
    plot <- plot +
    ggplot2::annotate(
      geom = "label",
      x = 0,
      y = Inf,
      label = paste(
        "Win odds (95% CI): ", round(win_odds[[1]], 2),
        " (", round(win_odds[[2]], 2), ", ", round(win_odds[[3]], 2), ")", "\n",
        "p-value: ", format.pval(win_odds[[4]], digits = 3, eps = 0.001),
        sep = ""
      ),
      hjust = 0, vjust = 1.4, size = 3
    )

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

  plot <- plot +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      ),
      axis.ticks.x.bottom = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.x.bottom =  ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = "none")

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

  tte_data <- utils::tail(
    utils::head(pb$data[[4]][, c("group", "x", "y")], -2), -2)
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
#' @return Used for side effect. Plots the maraca object.
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
    ...) {
  print(plot_maraca(
    x, continuous_grid_spacing_x, trans, density_plot_type, vline_type))
}
#' Generic function to plot the hce object using plot().
#'
#' @param x an object of S3 class 'hce'.
#' @param \dots not used
#' @param continuous_outcome A single string containing the continuous
#'                           outcome label. Default value "C".
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
#'                            (column TTEfixed). Otherwise,
#'                            this argument must be specified.
#'                            Note: If argument is specified and HCE object
#'                            contains TTEfixed column, then
#'                            fixed_followup_days argument is used.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#' @return Used for side effect. Plots the maraca object.
#'
#' @examples
#' set.seed(31337)
#' Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
#' Rates_P <- c(2.47, 2.24, 2.9, 4, 6)
#' HCE <- hce::simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P,
#'              CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3)
#' plot(HCE)
#' plot(HCE, fixed_followup_days = 3 * 365)
#'
#' @export
plot.hce <- function(x, continuous_outcome = "C",
                     continuous_grid_spacing_x = 10,
                     trans = "identity",
                     density_plot_type = "default",
                     vline_type = "median",
                     fixed_followup_days = NULL,
                     compute_win_odds = FALSE, ...) {
  checkmate::assert_string(continuous_outcome)
  checkmate::assert_int(continuous_grid_spacing_x)
  checkmate::assert_string(trans)
  checkmate::assert_choice(
    density_plot_type, c("default", "violin", "box", "scatter"))
  checkmate::assert_choice(
    vline_type, c("median", "mean", "none")
  )
  checkmate::assert_flag(compute_win_odds)
  checkmate::assertNames(names(x), must.include = "GROUP")

  x <- as.data.frame(x)
  TTE <- sort(unique(x$GROUP)[unique(x$GROUP) != continuous_outcome])

  if (is.null(fixed_followup_days)) {
    checkmate::assertNames(names(x), must.include = "TTEfixed")
    checkmate::assert_int(x$TTEfixed[[1]])
    fixed_followup_days <- x$TTEfixed[[1]]
  }

  hce_test <- maraca(
    data = x,
    tte_outcomes = TTE,
    continuous_outcome = continuous_outcome,
    column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
    arm_levels = c(active = "A", control = "P"),
    fixed_followup_days = fixed_followup_days,
    compute_win_odds = compute_win_odds
  )

  print(plot_maraca(
    hce_test, continuous_grid_spacing_x, trans, density_plot_type, vline_type))
}

### Private functions

# Computes the win odds from the internal data.
.compute_win_odds <- function(HCE) {
  HCE <- base::as.data.frame(HCE)
  HCE <- .with_ordered_column(HCE)
  fit <- hce::calcWO(x = HCE, AVAL = "ordered", TRTP = "arm", ref = "control")
  CI <- base::as.numeric(fit[, base::c("WO", "LCL", "UCL")])
  p <- fit$Pvalue
  win_odds <- base::c(CI, p)
  names(win_odds) <- base::c("estimate", "lower", "upper", "p-value")
  return(win_odds)
}


# This function does a bit of dirty magic to distribute the values
# onto different "floors", each floor being a numeric offset that is higher
# for each passing tte variable (and highest for the continuous).
# In practice, we are translating the values for each tte variable group.
# Explanation inline
.with_ordered_column <- function(HCE) {
  # We create a data frame, grouping according to the outcome,
  # then we get the minimum and maximum values of the value.
  # What we want to know is the "window" where data are for each of the groups
  # We then select the largest window.
  `%>%` <- dplyr::`%>%`

  tmp <- HCE %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(min = min(value), max = max(value)) %>%
    dplyr::mutate(separation = max - min) %>%
    dplyr::summarise(max_separation = max(separation))

  # With the largest window found, we know that if we offset the data at
  # least of this amount, they will never overlap. Bit of clever math here,
  # we use a gap that is larger, amounting to the number of digits, so we
  # have nicer gap value such as 10, 100, or 1000 etc.
  gap <- 10 ^ ceiling(log10(tmp$max_separation)) # nolint

  # apply the gap to all values. outcome is a factor, so we use its numeric
  # value to multiply the offset, and end up having each value "translated up"
  # of the proper amount.
  HCE <- HCE %>%
    dplyr::mutate(ordered = .env$gap * (as.numeric(outcome) - 1) + value)

  # and now we have a new data set with the column added.
  return(HCE)
}

# Computes the metainfo from the internal HCE data.
.compute_metainfo <- function(HCE) {
  n <- dplyr::n
  `%>%` <- dplyr::`%>%`

  meta1 <- HCE  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      n = n(),
      proportion = n / dim(HCE)[[1]] * 100,
      maxday = max(value, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      startx = c(0, cumsum(utils::head(proportion, -1))),
      endx = cumsum(proportion),
      starty = 0,
      n.groups = length(unique(outcome))
    )

  meta2 <- HCE  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome, arm) %>%
    dplyr::summarise(n = n(), proportion = n / dim(HCE)[[1]] * 100) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(n, proportion))

  meta_missing <- HCE %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      missing = sum(is.na(value))
    )

  meta <- dplyr::left_join(meta1, meta2, "outcome")
  meta <- dplyr::left_join(meta, meta_missing, "outcome")

  return(meta)
}

# Calculates the cumulative distribution for TTE outcomes
.compute_ecdf_by_outcome <- function(
  HCE, meta, tte_outcomes, continuous_outcome, arm_levels,
  fixed_followup_days
) {

  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  num_tte_outcomes <- length(tte_outcomes)
  HCE$t_cdf <- (num_tte_outcomes + 2) * fixed_followup_days

  for (i in seq_len(num_tte_outcomes)) {
    HCE[HCE$outcome == tte_outcomes[[i]], ]$t_cdf <-
      HCE[HCE$outcome == tte_outcomes[[i]], ]$value +
        fixed_followup_days * (i - 1)
  }

  HCE_ecdf <-
    do.call("rbind",
            lapply(unique(HCE$arm), function(a, df, outcomes) {
      tmp <- df %>% dplyr::filter(arm == a)
      tmp$ecdf_values <- 100 *
        stats::ecdf(tmp$t_cdf)(tmp$t_cdf)
      tmp %>% dplyr::filter(outcome %in% outcomes)
  }, df = HCE, outcomes = tte_outcomes))

  HCE_ecdf <- HCE_ecdf[order(HCE_ecdf$ecdf_values), ]

  HCE_ecdf$adjusted.time <- 0
  for (entry in tte_outcomes) {
    outcome_filter <- HCE_ecdf$outcome == entry
    HCE_ecdf[outcome_filter, ]$adjusted.time <-
      meta[meta$outcome == entry, ]$startx +
      HCE_ecdf[outcome_filter, ]$value /
      fixed_followup_days *
      meta[meta$outcome == entry, ]$proportion
  }

  HCE_ecdf_meta <- HCE_ecdf %>%
    dplyr::group_by(arm, outcome) %>%
    dplyr::summarise(
      max = max(ecdf_values, na.rm = TRUE),
      sum.event = n()) %>%
    dplyr::mutate(
      ecdf_end = utils::tail(max, 1)
    )

  return(list(
    data = HCE_ecdf,
    meta = HCE_ecdf_meta
  ))
}

# Support function for the range
.to_rangeab <- function(x, start_continuous_endpoint, minval, maxval) {
  (100 - start_continuous_endpoint) * (x - minval) /
  (maxval - minval) + start_continuous_endpoint
}

# Computes the continuous information
.compute_continuous <- function(
    HCE, meta, ecdf_mod, tte_outcomes, continuous_outcome, arm_levels) {
  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  continuous_data <- HCE[HCE$outcome == continuous_outcome, ]
  start_continuous_endpoint <- meta[meta$outcome == continuous_outcome, ]$startx

  continuous_data$x <- .to_rangeab(
    continuous_data$value,
    start_continuous_endpoint,
    min(continuous_data$value, na.rm = TRUE),
    max(continuous_data$value, na.rm = TRUE)
  )
  continuous_meta <- continuous_data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(n = n(), median = stats::median(x, na.rm = TRUE),
      average = base::mean(x, na.rm = TRUE))

  continuous_data$y_level <- ecdf_mod$meta[
    ecdf_mod$meta$arm == "active" &
    ecdf_mod$meta$outcome == utils::tail(tte_outcomes, 1),
    ]$ecdf_end
  continuous_data[continuous_data$arm == "control", ]$y_level <- ecdf_mod$meta[
    ecdf_mod$meta$arm == "control" &
    ecdf_mod$meta$outcome == utils::tail(tte_outcomes, 1),
    ]$ecdf_end

  return(list(
    data = continuous_data,
    meta = continuous_meta
  ))
}

# Reformats the data coming in from outside so that it fits our expectation.
.reformat_and_check_data <- function(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names) {
  `%>%` <- dplyr::`%>%`
  vars <- dplyr::vars
  all_of <- dplyr::all_of

  HCE <- data %>%
    dplyr::rename(all_of(column_names)) %>%
    dplyr::select(all_of(names(column_names)))

  # Check if the arm and outcome are strings, rather than factors.
  if (!inherits(HCE[, "arm"], "character")) {
    stop(paste(
      "The arm column must be characters.",
      "If you used read.csv, ensure you specify stringsAsFactors = FALSE."
    ))
  }

  if (!inherits(HCE[, "outcome"], "character")) {
    stop(paste(
      "The outcome column must be characters.",
      "If you used read.csv, ensure you specify stringsAsFactors = FALSE."
    ))
  }

  inverse_map <- stats::setNames(names(arm_levels), arm_levels)
  HCE$arm <- sapply(HCE$arm, function(x) {
    return(inverse_map[x])
  })

  endpoints <- c(tte_outcomes, continuous_outcome)
  HCE <- HCE %>%
    dplyr::filter(outcome %in% endpoints) %>%
    dplyr::mutate_at(vars(outcome), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(arm), factor, levels = names(arm_levels))

  # Check if the endpoints are all present
  for (entry in c(tte_outcomes, continuous_outcome)) {
    if (!any(HCE$outcome == entry)) {
      stop(paste(
        "Outcome", entry, "is not present in column",
        column_names[["outcome"]]
      ))
    }
  }

  return(HCE)

}

.minor_grid <- function(values, scale, continuous_grid_spacing_x) {
  minval <- min(values, na.rm = TRUE)
  maxval <- max(values, na.rm = TRUE)

  minor_grid_left <- c(0)
  if ((10^scale) * floor(minval * 10^(-scale)) < 0) {
    minor_grid_left <- rev(seq(
      0,
      (10^scale) * floor(minval * 10^(-scale)),
      by = -continuous_grid_spacing_x
    ))
  }

  minor_grid_right <- c(0)
  if ((10^scale) * ceiling(maxval * 10^(-scale)) > 0) {
    minor_grid_right <- seq(
      0,
      (10^scale) * ceiling(maxval * 10^(-scale)),
      by = continuous_grid_spacing_x
    )
  }
  minor_grid <- unique(c(minor_grid_left, minor_grid_right))
  minor_grid <- minor_grid[minor_grid >= minval & minor_grid <= maxval]

  return(minor_grid)
}
