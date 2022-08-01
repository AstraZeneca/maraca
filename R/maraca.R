#' Creates the maraca analysis object as an S3 object of class 'maraca::maraca'
#'
#' @param data A data frame with columns for the following information:
#'             - outcome column, containing the time-to-event and continuous
#'               labels
#'             - arm column, contaning the arm a given row belongs to.
#'             - ordered column, containing the continuous value
#'             - original column, containing the difference in values.
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
#'        outcome, arm, ordered and original to the associated column names
#'        in the data. The vector names must match in order "outcome", "arm",
#'        "ordered" and "original". Note that this parameter only need to be
#'        specified if you have column names different from the ones above.
#' @param fixed_followup_days The followup days, or NULL.
#'
#' @export
maraca <- function(
    data,
    tte_outcomes,
    continuous_outcome,
    arm_levels = c(
      active = "Active",
      control = "Control"
    ),
    column_names = c(
      outcome = "outcome", arm = "arm",
      ordered = "ordered", original = "original"
    ),
    fixed_followup_days = NULL
    ) {

  checkmate::assert_data_frame(data)
  checkmate::assert_character(tte_outcomes, len = 4, any.missing = FALSE)
  checkmate::assert_string(continuous_outcome)
  checkmate::assert_character(arm_levels, len = 2, any.missing = FALSE)
  checkmate::assert_names(
    names(arm_levels),
    identical.to = c("active", "control")
  )
  checkmate::assert_int(fixed_followup_days, null.ok = TRUE)
  checkmate::assert_character(column_names, len = 4, any.missing = FALSE)
  checkmate::assert_names(
    names(column_names),
    identical.to = c("outcome", "arm", "ordered", "original")
  )

  # Check if the arm and outcome are strings, rather than factors.
  # Remove unwanted outcomes and arm levels, and normalise column names
  # in the internal data.
  HCE <- .reformat_data(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names)

  # Calculate meta information from the entire HCE dataset needed for plotting
  meta <- .compute_metainfo(HCE)
  survmod <- .compute_survmod(
    HCE, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days
  )
  slope <- .compute_slope(
    HCE, meta, survmod, tte_outcomes, continuous_outcome, arm_levels
  )
  win_odds <- .compute_win_odds(HCE)

  return(
    structure(
      list(
        tte_outcomes = tte_outcomes,
        continuous_outcome = continuous_outcome,
        arm_levels = arm_levels,
        fixed_followup_days = fixed_followup_days,
        column_names = column_names,
        meta = meta,
        slope = slope,
        survmod = survmod,
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
#'
#' @export
plot_maraca <- function(obj) {
  checkmate::assert_class(obj, "maraca::maraca")
  aes <- ggplot2::aes

  meta <- obj$meta
  slope <- obj$slope
  survmod <- obj$survmod
  win_odds <- obj$win_odds
  start_continuous_endpoint <- meta[
    meta$outcome == obj$continuous_outcome, ]$startx

  minor_grid <- seq(
    sign(min(slope$data$original)) *
      floor(abs(min(slope$data$original)) / 10) * 10,
    sign(max(slope$data$original)) *
      floor(abs(max(slope$data$original)) / 10) * 10,
    by = 10
  )

  zeroposition <- .to_rangeab(0,
    start_continuous_endpoint,
    min(slope$data$original),
    max(slope$data$original)
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
    ) +
    ggplot2::geom_vline(
      xintercept = slope$meta$median,
      color = c("#F8766D", "#00BFC4"), linetype = "dashed", size = 0.3
    ) +
    ggplot2::geom_line(
      data = slope$data,
      aes(x = violinx, y = violiny, color = arm)
    ) +
    ggplot2::geom_line(
      data = survmod$data,
      aes(x = adjusted.time, y = km.start + km.y * 100, color = strata)
    ) +
    ggplot2::geom_violin(
      data = slope$data,
      aes(x = x, y = violiny, fill = factor(violiny)), alpha = 0.5
    ) +
    ggplot2::geom_boxplot(
      data = slope$data,
      aes(x = x, y = violiny, fill = factor(violiny)), alpha = 0.5, width = 2
    ) +
    ggplot2::xlab("Type of endpoint") +
    ggplot2::ylab("Cumulative proportion") +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(meta$proportion / 2 + meta$startx),
      labels = c(obj$tte_outcomes, obj$continuous_outcome),
      minor_breaks = .to_rangeab(
        minor_grid,
        start_continuous_endpoint,
        min(slope$data$original),
        max(slope$data$original)
      )
    ) +
    ggplot2::annotate(
      geom = "text",
      x = .to_rangeab(
        minor_grid,
        start_continuous_endpoint,
        min(slope$data$original),
        max(slope$data$original)
        ),
      y = 0,
      label = minor_grid, color = "grey60"
    ) +
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
    ) +
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
#' @param ... not used
#'
#' @export
`plot.maraca::maraca` <- function(x, ...) {
  print(plot_maraca(x))
}

# Private functions

.compute_win_odds <- function(HCE) {
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

.compute_metainfo <- function(HCE) {
  n <- dplyr::n
  `%>%` <- dplyr::`%>%`

  meta1 <- HCE %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      n = n(), proportion = n / dim(HCE)[1] * 100, maxday = max(original)) %>%
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

.compute_survmod <- function(
    HCE, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days
  ) {
  endpoints <- c(tte_outcomes, continuous_outcome)
  vars <- dplyr::vars
  `%>%` <- dplyr::`%>%`

  if (is.null(fixed_followup_days)) {
    # Use the largest value across the hard outcomes if i
    # fixed.follow.up.days is not specified
    HCE$kmday <- max(meta[meta$outcome %in% tte_outcomes, ]$maxday)
  } else {
    # Use the specified length of the fixed-follow-up trial if specified
    HCE$kmday <- fixed_followup_days
  }

  HCE[HCE$outcome %in% tte_outcomes, ]$kmday <- HCE[
      HCE$outcome %in% tte_outcomes, ]$original

  Surv <- survival::Surv # nolint

  for (i in seq_along(tte_outcomes)) {
    # Create survival model dataset
    survmod_data_row <- cbind(
      ggplot2::fortify(with(HCE,
        survival::survfit(
          Surv(time = kmday, event = outcome == tte_outcomes[i]) ~ arm))
      ), outcome = tte_outcomes[i])

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
  for (i in tte_outcomes) {
    survmod_data[survmod_data$outcome == i, ]$adjusted.time <- meta[
      meta$outcome == i, ]$startx +
      survmod_data[survmod_data$outcome == i, ]$time /
      max(survmod_data[survmod_data$outcome == i, ]$time) *
      meta[meta$outcome == i, ]$proportion
  }

  survmod_data <- survmod_data %>% dplyr::mutate(km.y = 1 - surv)

  survmod_meta <- survmod_data %>%
    dplyr::group_by(strata, outcome) %>%
    dplyr::summarise(max = 100 * max(1 - surv), sum.event = sum(n.event)) %>%
    dplyr::mutate(km.start = c(0, cumsum(utils::head(max, -1))),
      km.end = cumsum(max))

  survmod_data <- survmod_data %>% dplyr::left_join(
    survmod_meta, by = c("strata", "outcome")
  )

  return(list(
    data = survmod_data,
    meta = survmod_meta
  ))
}

.to_rangeab <- function(x, start_continuous_endpoint, minval, maxval) {
    (100 - start_continuous_endpoint) * (x - minval) /
    (maxval - minval) + start_continuous_endpoint
}

.compute_slope <- function(
    HCE, meta, survmod, tte_outcomes, continuous_outcome, arm_levels) {
  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  slope_data <- HCE[HCE$outcome == continuous_outcome, ]
  start_continuous_endpoint <- meta[meta$outcome == continuous_outcome, ]$startx

  slope_data$x <- .to_rangeab(
    slope_data$original,
    start_continuous_endpoint,
    min(slope_data$original),
    max(slope_data$original)
  )
  slope_meta <- slope_data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(n = n(), median = stats::median(x),
      average = base::mean(x))

  slope_data$violinx <- 0
  slope_data[slope_data$arm == "active", ]$violinx <- seq(
    from = start_continuous_endpoint, to = 100,
    length.out = slope_meta$n[slope_meta$arm == "active"])
  slope_data[slope_data$arm == "control", ]$violinx <- seq(
    from = start_continuous_endpoint, to = 100,
    length.out = slope_meta$n[slope_meta$arm == "control"])

  slope_data$violiny <- survmod$meta[
    survmod$meta$strata == "active" &
    survmod$meta$outcome == utils::tail(tte_outcomes, 1),
    ]$km.end
  slope_data[slope_data$arm == "control", ]$violiny <- survmod$meta[
    survmod$meta$strata == "control" &
    survmod$meta$outcome == utils::tail(tte_outcomes, 1),
    ]$km.end

  return(list(
    data = slope_data,
    meta = slope_meta
  ))
}

.reformat_data <- function(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names) {
  `%>%` <- dplyr::`%>%`
  vars <- dplyr::vars
  all_of <- dplyr::all_of
  HCE <- data %>%
    dplyr::rename(all_of(column_names)) %>%
    dplyr::select(all_of(names(column_names)))

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

  inverse_map <- setNames(names(arm_levels), arm_levels)
  HCE$arm <- sapply(HCE$arm, function(x) {
    return(inverse_map[x])
  })

  endpoints <- c(tte_outcomes, continuous_outcome)
  return(HCE %>%
    dplyr::filter(outcome %in% endpoints) %>%
    dplyr::mutate_at(vars(outcome), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(arm), factor, levels = names(arm_levels))
  )
}
