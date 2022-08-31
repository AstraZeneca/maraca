#' Creates the maraca analysis object as an S3 object of class 'maraca::maraca'
#'
#' @param data A data frame with columns for the following information:
#'             - outcome column, containing the time-to-event and continuous
#'               labels
#'             - arm column, contaning the arm a given row belongs to.
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
#' @param fixed_followup_days The followup days, or NULL. If NULL, use the
#'        largest value across the hard outcomes.
#' @param compute_win_odds If TRUE compute the win odds, otherwise (default)
#'                         don't compute them.
#'
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
    fixed_followup_days = NULL,
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
  checkmate::assert_int(fixed_followup_days, null.ok = TRUE)
  checkmate::assert_flag(compute_win_odds)

  # Remove unwanted outcomes and arm levels, and normalise column names
  # in the internal data.
  # Note: We use HCE to refer to our internal, normalised data frame.
  # and with "data" to the user-provided, external, dirty data frame.
  HCE <- .reformat_and_check_data(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names)

  # Calculate meta information from the entire HCE dataset needed for plotting
  meta <- .compute_metainfo(HCE)
  survmod <- .compute_survmod(
    HCE, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days
  )
  continuous <- .compute_continuous(
    HCE, meta, survmod, tte_outcomes, continuous_outcome, arm_levels
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
        survmod = survmod,
        continuous = continuous,
        win_odds = win_odds
      ),
      class = c("maraca::maraca")
    )
  )
}

#' Creates and returns the plot of the maraca data.
#'
#' Renders and returns a ggplot2 object of the maraca data. This function
#' will not render the plot immediately. You have to print() the returned
#' object for it to be displayed.
#'
#' @param obj an object of S3 class 'maraca::maraca'
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the data before plotting.
#'        The accepted values are the same that ggplot2::scale_x_continuous
#' @param density_plot_type which type of plot to display in the continuous
#'        part of the plot. Options are "default", "violin", "box", "scatter".
#' @param vline_type what the vertical lines in the continuous part of the plot
#'        should highlight. Options are "median", "mean", "none".
#'
#' @export
plot_maraca <- function(
    obj, continuous_grid_spacing_x = 10, trans = "identity",
    density_plot_type = "default",
    vline_type = "median") {
  checkmate::assert_class(obj, "maraca::maraca")
  checkmate::assert_choice(
    density_plot_type, c("default", "violin", "box", "scatter")
  )
  checkmate::assert_choice(
    vline_type, c("median", "mean", "none")
  )
  aes <- ggplot2::aes

  meta <- obj$meta
  continuous <- obj$continuous
  survmod <- obj$survmod
  win_odds <- obj$win_odds
  start_continuous_endpoint <- meta[
    meta$outcome == obj$continuous_outcome, ]$startx

  minor_grid <- seq(
    sign(min(continuous$data$value, na.rm = TRUE)) *
      floor(abs(min(continuous$data$value, na.rm = TRUE)) / 10) * 10,
    sign(max(continuous$data$value, na.rm = TRUE)) *
      floor(abs(max(continuous$data$value, na.rm = TRUE)) / 10) * 10,
    by = continuous_grid_spacing_x
  )

  zeroposition <- .to_rangeab(0,
    start_continuous_endpoint,
    min(continuous$data$value, na.rm = TRUE),
    max(continuous$data$value, na.rm = TRUE)
  )
  # Plot the information in the Maraca plot
  plot <- ggplot2::ggplot(survmod$data, aes(colour = arm)) +
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
    ggplot2::geom_line(
      data = continuous$data,
      aes(x = violinx, y = violiny, color = arm)
    ) +
    ggplot2::geom_line(
      data = survmod$data,
      aes(x = adjusted.time, y = km.start + km.y * 100, color = strata)
    )

  if (density_plot_type == "default" || density_plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(
        data = continuous$data,
        aes(x = x, y = violiny, fill = factor(violiny)), alpha = 0.5
      )
  }

  if (density_plot_type == "default" || density_plot_type == "box") {
    plot <- plot +
      ggplot2::geom_boxplot(
        data = continuous$data,
        aes(x = x, y = violiny, fill = factor(violiny)), alpha = 0.5, width = 2
      )
  }

  if (density_plot_type == "scatter") {
    plot <- plot +
      ggplot2::geom_jitter(data = continuous$data, aes(x = x, y = violiny))
  }

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
      label = minor_grid, color = "grey60"
    )

  if (!is.null(win_odds)) {
    plot <- plot +
    ggplot2::annotate(
      geom = "label",
      x = 0,
      y = Inf,
      label = paste(
        "Win odds (95% CI): ", round(win_odds[1], 2),
        " (", round(win_odds[2], 2), ", ", round(win_odds[3], 2), ")", "\n",
        "p-value: ", format.pval(win_odds[4], digits = 3, eps = 0.001),
        sep = ""
      ),
      hjust = 0, vjust = 1.4, size = 3
    )
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

  return(plot)
}

#' Creates and returns the tte trellis plot of the maraca data.
#'
#' Renders and returns a ggplot2 object of the data. This function
#' will not render the plot immediately. You have to print() the returned
#' object for it to be displayed.
#'
#' @param obj an object of S3 class 'maraca::maraca'
#'
#' @export
plot_tte_trellis <- function(obj) {
  checkmate::assert_class(obj, "maraca::maraca")

  aes <- ggplot2::aes
  vars <- dplyr::vars

  survmod <- obj$survmod
  plot <- ggplot2::ggplot(survmod$data) +
    ggplot2::geom_line(aes(x = time, y = km.y * 100, color = strata)) +
    ggplot2::facet_grid(cols = vars(outcome))
  return(plot)
}

#' Generic function to plot the maraca object using plot().
#'
#' This will produce the plot_maraca plot.
#'
#' @param x an object of S3 class 'maraca::maraca'
#' @param \dots not used
#' @param continuous_grid_spacing_x The spacing of the x grid to use for the
#'        continuous section of the plot.
#' @param trans the transformation to apply to the data before plotting.
#'        The accepted values are the same that ggplot2::scale_x_continuous
#'
#' @export
`plot.maraca::maraca` <- function(
    x, continuous_grid_spacing_x = 10, trans = "identity",
    density_plot_type = "default",
    vline_type = "median",
    ...) {
  print(plot_maraca(
    x, continuous_grid_spacing_x, trans, density_plot_type, vline_type))
}

### Private functions

# Computes the win odds from the internal data.
.compute_win_odds <- function(HCE) {
  HCE <- .with_ordered_column(HCE)
  grp <- sanon::grp # nolint
  fit <- sanon::sanon(
    ordered ~ grp(arm, ref = "control"),
    data = HCE)
  CI0 <- stats::confint(fit)$ci
  CI <- CI0 / (1 - CI0)
  p <- fit$p

  win_odds <- c(CI, p)
  names(win_odds) <- c("estimate", "lower", "upper", "p-value")

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

  meta1 <- HCE %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      n = n(),
      proportion = n / dim(HCE)[1] * 100,
      maxday = max(value, na.rm = TRUE)) %>%
    dplyr::mutate(
      startx = c(0, cumsum(utils::head(proportion, -1))),
      endx = cumsum(proportion),
      starty = 0,
      n.groups = length(unique(outcome))
    )

  meta2 <- HCE %>%
    dplyr::group_by(outcome, arm) %>%
    dplyr::summarise(n = n(), proportion = n / dim(HCE)[1] * 100) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(n, proportion))

  meta <- dplyr::left_join(meta1, meta2, "outcome")

  return(meta)
}

# Performs the survmod calculation
.compute_survmod <- function(
    HCE, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days
  ) {
  endpoints <- c(tte_outcomes, continuous_outcome)
  vars <- dplyr::vars
  `%>%` <- dplyr::`%>%`

  if (is.null(fixed_followup_days)) {
    # Use the largest value across the hard outcomes if
    # fixed_followup_days is not specified
    fixed_followup_days <- max(meta[meta$outcome %in% tte_outcomes, ]$maxday)
  }

  Surv <- survival::Surv # nolint

  for (i in seq_along(tte_outcomes)) {
    HCE_focused <- .hce_survival_focus(
      HCE, i, tte_outcomes, fixed_followup_days)

    # Create survival model dataset
    survmod_data_row <- cbind(
      ggplot2::fortify(
        with(HCE_focused,
          survival::survfit(
            Surv(time = kmday, event = outcome == tte_outcomes[i]) ~ arm))
        ),
        outcome = tte_outcomes[i]
      )

    n <- dplyr::n
    # remove first and last point
    survmod_data_row <- survmod_data_row

    if (i == 1) {
      survmod_data_row <- survmod_data_row %>%
        dplyr::group_by(strata) %>%
        dplyr::slice(1:(n() - 1))
    } else if (i == length(tte_outcomes)) {
      survmod_data_row <- survmod_data_row %>%
        dplyr::group_by(strata) %>%
        dplyr::slice(2:(n() - 1))
    } else {
      survmod_data_row <- survmod_data_row %>%
        dplyr::group_by(strata) %>%
        dplyr::slice(2:(n() - 1))
    }

    if (i == 1) {
      survmod_data <- survmod_data_row
    } else {
      survmod_data <- rbind(
        survmod_data,
        survmod_data_row
      )
    }
  }

  survmod_data <- survmod_data %>%
    dplyr::mutate_at(vars(outcome), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(strata), factor, levels = names(arm_levels))

  survmod_data$adjusted.time <- 0
  for (entry in tte_outcomes) {
    outcome_filter <- survmod_data$outcome == entry
    survmod_data[outcome_filter, ]$adjusted.time <-
        meta[meta$outcome == entry, ]$startx +
        survmod_data[outcome_filter, ]$time /
        max(survmod_data[outcome_filter, ]$time, na.rm = TRUE) *
        meta[meta$outcome == entry, ]$proportion
  }

  survmod_data <- survmod_data %>% dplyr::mutate(km.y = 1 - surv)

  survmod_meta <- survmod_data %>%
    dplyr::group_by(strata, outcome) %>%
    dplyr::summarise(
      max = 100 * max(1 - surv, na.rm = TRUE),
      sum.event = sum(n.event, na.rm = TRUE)) %>%
    dplyr::mutate(
      km.start = c(0, cumsum(utils::head(max, -1))),
      km.end = cumsum(max)
    )

  # We put an additional point on both curves to match the continuous
  # horizontal line no matter which point is rightmost in the last outcome.
  # This way the junction appears smooth
  add_points <- survmod_data %>%
    dplyr::group_by(strata) %>%
    dplyr::slice_tail(n = 1)

  # We use the endx point so we are sure that it's the highest we can get in
  # x terms that matches the grey line. Note also that we add the point
  # after we calculated the meta information.
  add_points$adjusted.time <- meta[
    meta$outcome == utils::tail(tte_outcomes, 1), ]$endx
  survmod_data <- rbind(
    survmod_data,
    add_points
  )

  survmod_data <- survmod_data %>% dplyr::left_join(
    survmod_meta, by = c("strata", "outcome")
  )

  return(list(
    data = survmod_data,
    meta = survmod_meta
  ))
}

# Support function for the range
.to_rangeab <- function(x, start_continuous_endpoint, minval, maxval) {
  (100 - start_continuous_endpoint) * (x - minval) /
  (maxval - minval) + start_continuous_endpoint
}

.hce_survival_focus <- function(HCE, idx, tte_outcomes, fixed_followup_days) {
  # We take the pre, and post entries in the sequence,
  # e.g. with tte_outcomes being A,B,C,D,E,F and with index i pointing at
  # C, pre will contain A, B
  pre_outcomes <- tte_outcomes[seq_len(idx - 1)]

  HCE$kmday <- fixed_followup_days
  pre_row_mask <- (HCE$outcome %in% pre_outcomes)
  HCE[pre_row_mask, "kmday"] <- 0
  tte_row_mask <- (HCE$outcome == tte_outcomes[[idx]])
  HCE[tte_row_mask, "kmday"] <- HCE[tte_row_mask, ]$value
  return(HCE)
}


# Computes the continuous information
.compute_continuous <- function(
    HCE, meta, survmod, tte_outcomes, continuous_outcome, arm_levels) {
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

  continuous_data$violinx <- 0
  continuous_data[continuous_data$arm == "active", ]$violinx <- seq(
    from = start_continuous_endpoint, to = 100,
    length.out = continuous_meta$n[continuous_meta$arm == "active"])
  continuous_data[continuous_data$arm == "control", ]$violinx <- seq(
    from = start_continuous_endpoint, to = 100,
    length.out = continuous_meta$n[continuous_meta$arm == "control"])

  continuous_data$violiny <- survmod$meta[
    survmod$meta$strata == "active" &
    survmod$meta$outcome == utils::tail(tte_outcomes, 1),
    ]$km.end
  continuous_data[continuous_data$arm == "control", ]$violiny <- survmod$meta[
    survmod$meta$strata == "control" &
    survmod$meta$outcome == utils::tail(tte_outcomes, 1),
    ]$km.end

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
  if (class(HCE[, "arm"]) != "character") {
    stop(paste(
      "The arm column must be characters.",
      "If you used read.csv, ensure you specify stringsAsFactors = FALSE."
    ))
  }

  if (class(HCE[, "outcome"]) != "character") {
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
