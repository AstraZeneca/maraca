.run_plot_checks <- function(obj, continuous_grid_spacing_x, remove_outliers,
                             trans) {
  checkmate::assert_class(obj, "maraca")

  if (!(is.null(continuous_grid_spacing_x) ||
          is.numeric(continuous_grid_spacing_x))) {
    stop("continuous_grid_spacing_x has to be numeric or NULL")
  }

  checkmate::assert_flag(remove_outliers)
  checkmate::assert_string(trans)
  checkmate::assert_subset(trans,
                           choices = c("identity", "log", "log10",
                                       "sqrt", "reverse"),
                           empty.ok = FALSE)

  if (obj$last_type == "binary" && trans %in% c("log", "log10", "sqrt")) {
    stop(paste(trans, "transformation only implemented for continuous",
               "last endpoint."))
  }
}

.calculate_violin_stats <- function(data, start_last_endpoint) {

  arm_info <- unique(data[, c("arm", "y")])

  width <- diff(range(data$y)) * 0.9

  df_arm1 <- data[data$y == arm_info[1, ]$y, ]
  df_arm2 <- data[data$y == arm_info[2, ]$y, ]

  density1 <- stats::density(df_arm1$value, n = 512, bw = "nrd0",
                             adjust = 1, kernel = "gaussian",
                             from = min(df_arm1$value), to = max(df_arm1$value))

  density2 <- stats::density(df_arm2$value, n = 512, bw = "nrd0",
                             adjust = 1, kernel = "gaussian",
                             from = min(df_arm2$value), to = max(df_arm2$value))

  quantiles_arm1 <- unname(stats::quantile(df_arm1$value,
                                           probs = c(0.25, 0.5, 0.75)))
  quantiles_arm2 <- unname(stats::quantile(df_arm2$value,
                                           probs = c(0.25, 0.5, 0.75)))

  density_quants1 <- stats::approx(density1$x, density1$y,
                                   xout = quantiles_arm1, ties = "ordered")$y
  density_quants2 <- stats::approx(density2$x, density2$y,
                                   xout = quantiles_arm2, ties = "ordered")$y

  density_df <- data.frame(
    "value" = c(density1$x, quantiles_arm1, density2$x, quantiles_arm2),
    "y" = c(density1$y, density_quants1, density2$y, density_quants2),
    "arm_y" = rep(c(arm_info[1, ]$y, arm_info[2, ]$y), each = 515),
    "arm" = rep(as.character(c(arm_info[1, ]$arm, arm_info[2, ]$arm)),
                each = 515)
  )

  density_df$x <- .to_rangeab(density_df$value, start_last_endpoint,
                              min(data$value), max(data$value))

  density_df <- density_df[order(density_df$arm, density_df$x), ]
  density_df_lower <- density_df[order(density_df$arm, density_df$x,
                                       decreasing = c(FALSE, TRUE),
                                       method = "radix"), ]

  density_df$violinwidth <- density_df$arm_y +
    (density_df$y / max(density_df$y) * (width / 2))
  density_df_lower$violinwidth <- density_df_lower$arm_y -
    (density_df_lower$y / max(density_df_lower$y) * (width / 2))

  density_df <- rbind(density_df,
                      density_df_lower)

  density_df$y <- density_df$violinwidth
  density_df$outcome <- unique(data$outcome)
  density_df$type <- "violin"

  return(list("data" = density_df[, c("outcome", "arm", "value", "x",
                                      "y", "type")],
              "scaling_factor" = max(density_df$y)))
}

.calculate_boxplot_stats <- function(data) {

  df_list <- lapply(unique(data$y), function(y) {
    tmp <- data[data$y == y, ]
    qs <- c(0, 0.25, 0.5, 0.75, 1)
    stats <- as.numeric(stats::quantile(tmp$x, qs))
    names(stats) <- c("xmin", "xlower", "xmiddle", "xupper", "xmax")
    iqr <- diff(stats[c(2, 4)])
    lower_end <- stats[2] - 1.5 * iqr
    upper_end <- stats[4] + 1.5 * iqr
    outliers <- tmp$x < lower_end | tmp$x > upper_end
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], tmp$x[!outliers]), na.rm = TRUE)
    }

    stat_df <- data.frame(t(stats),
                          "lower_end" = lower_end,
                          "upper_end" = upper_end,
                          "y" = y,
                          "arm" = unique(tmp$arm))
    if (any(outliers)) {
      outlier_df <- data.frame("x" = tmp$x[outliers],
                               "y" = y,
                               "arm" = unique(tmp$arm))
    } else {
      outlier_df <- NULL
    }
    return(list("stats" = stat_df, "outlier" = outlier_df))
  })

  boxplot_stats <- do.call("rbind", lapply(df_list, function(x) {
    x$stats
  }))
  outlier_stats <- do.call("rbind", lapply(df_list, function(x) {
    x$outlier
  }))

  return(list("stats" = boxplot_stats, "outlier" = outlier_stats))

}

.rescale_boxplot_stats <- function(boxplot_data, start_x,
                                   current_min, current_max) {

  boxplot_data$xmin <- .to_rangeab(boxplot_data$xmin, start_x,
                                   current_min, current_max)
  boxplot_data$xlower <- .to_rangeab(boxplot_data$xlower, start_x,
                                     current_min, current_max)
  boxplot_data$xmiddle <- .to_rangeab(boxplot_data$xmiddle, start_x,
                                      current_min, current_max)
  boxplot_data$xupper <- .to_rangeab(boxplot_data$xupper, start_x,
                                     current_min, current_max)
  boxplot_data$xmax <- .to_rangeab(boxplot_data$xmax, start_x,
                                   current_min, current_max)

  return(boxplot_data)
}

.reverse_boxplot_stats <- function(boxplot_data, start_x) {

  boxplot_data$xmin <- start_x - boxplot_data$xmin + 100
  boxplot_data$xlower <- start_x - boxplot_data$xlower + 100
  boxplot_data$xmiddle <- start_x - boxplot_data$xmiddle + 100
  boxplot_data$xupper <- start_x - boxplot_data$xupper + 100
  boxplot_data$xmax <- start_x - boxplot_data$xmax + 100

  return(boxplot_data)
}

.trans_boxplot_stats <- function(boxplot_data, trans) {

  boxplot_data$xmin <- eval(parse(text = paste0(trans,
                                                "(boxplot_data$xmin)")))
  boxplot_data$xlower <- eval(parse(text = paste0(trans,
                                                  "(boxplot_data$xlower)")))
  boxplot_data$xmiddle <- eval(parse(text = paste0(trans,
                                                   "(boxplot_data$xmiddle)")))
  boxplot_data$xupper <- eval(parse(text = paste0(trans,
                                                  "(boxplot_data$xupper)")))
  boxplot_data$xmax <- eval(parse(text = paste0(trans,
                                                "(boxplot_data$xmax)")))

  return(boxplot_data)
}

.assign_vline_type <- function(last_type, density_plot_type, vline_type) {
  switch(last_type,
         "continuous" = .checks_continuous_outcome(density_plot_type,
                                                   vline_type),
         "binary" = .checks_binary_outcome(density_plot_type,
                                           vline_type),
         stop("Unsupported last outcome type"))
}

.prepare_ecdf_plot_data <- function(obj, step_outcomes) {

  `%>%` <- dplyr::`%>%`

  ecdf_mod <- obj$ecdf_by_outcome

  plotdata_ecdf <- ecdf_mod$data[, c("outcome", "arm", "value",
                                     "adjusted.time", "step_values",
                                     "type")]
  names(plotdata_ecdf) <- c("outcome", "arm", "value", "x", "y", "type")

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
  if (length(step_outcomes) > 1) {
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
  }

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

  return(plotdata_ecdf)
}


.prepare_continuous_plot_data <- function(plotdata_last, last_meta, trans,
                                          density_plot_type, remove_outliers,
                                          start_last_endpoint) {

  if (trans %in% c("log", "log10", "sqrt")) {

    if (min(plotdata_last$value) < 0) {
      if (density_plot_type %in% c("default", "violin", "box")) {
        stop(paste("Continuous endpoint has negative values - the",
                   trans, "transformation cannot be accurately calculated."))
      } else {
        warning(paste("Continuous endpoint has negative values - the",
                      trans, "transformation will result in missing values."))
      }
    }

    plotdata_last$value <- eval(parse(text = paste0(trans,
                                                    "(plotdata_last$value)")))

    range <- c(min(plotdata_last$value, na.rm = TRUE),
               max(plotdata_last$value, na.rm = TRUE))
    plotdata_last$x <- .to_rangeab(plotdata_last$value, start_last_endpoint,
                                   range[1], range[2])
  }

  boxplot_data <- .calculate_boxplot_stats(plotdata_last)

  if (density_plot_type %in% c("default", "violin")) {
    violin_list <- .calculate_violin_stats(plotdata_last, start_last_endpoint)
    plotdata_last <- violin_list$data
    violin_scaling_factor <- violin_list$scaling_factor
  } else {
    violin_scaling_factor <- NULL
  }

  if (remove_outliers && !is.null(boxplot_data$outlier)) {
    plotdata_last <-
      do.call("rbind", lapply(boxplot_data$stats$arm, function(trt) {
        tmp <- plotdata_last[plotdata_last$arm == trt, ]
        box_tmp <- boxplot_data$stats[boxplot_data$stats$arm == trt, ]
        tmp[tmp$x >= box_tmp$xmin & tmp$x <= box_tmp$xmax, ]
      }))

    current_min <- min(boxplot_data$stats$xmin, na.rm = TRUE)
    current_max <- max(boxplot_data$stats$xmax, na.rm = TRUE)

    if (density_plot_type %in% c("default", "box")) {
      boxplot_data$stats <- .rescale_boxplot_stats(boxplot_data$stats,
                                                   start_last_endpoint,
                                                   current_min, current_max)
      boxplot_data$outlier <- NULL
    }

    plotdata_last$x <- .to_rangeab(plotdata_last$x, start_last_endpoint,
                                   current_min, current_max)

    last_meta$median <- .to_rangeab(last_meta$median,
                                    start_last_endpoint,
                                    current_min, current_max)
    last_meta$average <- .to_rangeab(last_meta$average,
                                     start_last_endpoint,
                                     current_min, current_max)
  }

  return(list("plotdata_last" = plotdata_last, "boxplot_data" = boxplot_data,
              "violin_scaling_factor" = violin_scaling_factor,
              "last_meta" = last_meta))
}


.create_grid <- function(plotdata_last, last_type, trans, last_meta,
                         scale, continuous_grid_spacing_x) {
  if (last_type == "continuous") {
    range <- c(min(plotdata_last$value, na.rm = TRUE),
               max(plotdata_last$value, na.rm = TRUE))

    if (trans %in% c("log", "log10", "sqrt")) {

      range <- .untransform_range(trans, range)
      minor_grid <- switch(trans,
                           "log" = .logTicks(range),
                           "log10" = .log10Ticks(range),
                           "sqrt" = pretty(range))
      minor_grid <- minor_grid[minor_grid >= range[1] &
                                 minor_grid <= range[2]]
      minor_grid_x <- eval(parse(text = paste0(trans, "(minor_grid)")))
    } else {
      minor_grid <- .minor_grid(plotdata_last$value, scale,
                                continuous_grid_spacing_x)
      minor_grid_x <- minor_grid
    }

  } else if (last_type == "binary") {

    lowest_value <- last_meta$estimate - last_meta$ci_diff
    highest_value <- last_meta$estimate + last_meta$ci_diff
    range <- c(min(0, floor(lowest_value / 10) * 10),
               max(100, ceiling(highest_value / 10) * 10))
    minor_grid <- seq(range[1], range[2], continuous_grid_spacing_x)
    minor_grid_x <- minor_grid

  }
  return(list("range" = range, "minor_grid" = minor_grid,
              "minor_grid_x" = minor_grid_x))
}

.prepare_vline_data <- function(last_meta, vline_type, trans) {

  `%>%` <- dplyr::`%>%`

  vline_data <- NULL
  if (vline_type == "median") {
    vline_data <- last_meta %>%
      dplyr::select("x" = median, arm)
  } else if (vline_type == "mean") {
    vline_data <- last_meta %>%
      dplyr::select("x" = average, arm)
  }

  if (trans %in% c("log", "log10", "sqrt")) {
    if (!is.null(vline_data)) {
      vline_data$x <- eval(parse(text = paste0(trans, "(vline_data$x)")))
    }
  }

  return(vline_data)
}

.set_up_initial_plot <- function(plotdata, meta, vline_data) {

  plot <- ggplot2::ggplot(plotdata) +
    ggplot2::geom_vline(
      xintercept = cumsum(c(0, meta$proportion)),
      color = "grey80"
    )

  if (!is.null(vline_data)) {
    plot <- plot +
      ggplot2::geom_vline(
        mapping = ggplot2::aes(
          xintercept = x,
          color = arm
        ),
        data = vline_data,
        linetype = "dashed",
        linewidth = 0.8,
        show.legend = FALSE
      )
  }

  return(plot)
}

.add_binary_steps_to_plot <- function(plot, plotdata, step_outcomes,
                                      step_types, which_binary) {

  `%>%` <- dplyr::`%>%`

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

  return(plot)
}

.add_binary_steps_to_animation <- function(plot, plotdata, step_outcomes,
                                           step_types, which_binary,
                                           speed_factor) {

  `%>%` <- dplyr::`%>%`

  tmp <- plotdata[plotdata$outcome %in% step_outcomes[which_binary], ]
  tmp <- tmp[order(tmp$x), ]

  if (step_types[length(step_types)] == "binary") {
    tmp <- dplyr::slice_head(tmp, n = -2)
  }

  for (o in step_outcomes[which_binary]) {
    tmp1 <- tmp %>%
      dplyr::filter(outcome  == o) %>%
      dplyr::group_by(outcome, arm) %>%
      dplyr::summarize("xend" = max(x),
                       "x" = min(x),
                       "y" = min(y),
                       "time" = unique(time)) %>%
      dplyr::ungroup()

    tmp2 <- tmp %>%
      dplyr::filter(outcome  == o) %>%
      dplyr::group_by(outcome, arm) %>%
      dplyr::summarize("x" = max(x),
                       "yend" = max(y),
                       "y" = min(y)) %>%
      dplyr::ungroup()

    tmp2 <- dplyr::left_join(tmp2, tmp1 %>% dplyr::select(arm, time),
                             by = "arm")
    tmp2$time <- tmp2$time + 5 * speed_factor

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

  return(plot)
}

.add_end_binary_step <- function(plot, plotdata, step_outcomes,
                                 animation = FALSE) {

  `%>%` <- dplyr::`%>%`

  tmp <- plotdata %>%
    dplyr::filter(outcome == utils::tail(step_outcomes, 1)) %>%
    dplyr::group_by(arm) %>%
    dplyr::slice_tail(n = -1) %>%
    dplyr::summarize("xend" = max(x),
                     "x" = min(x),
                     "y" = max(y)) %>%
    dplyr::ungroup()

  if (animation) {
    tmp <- plotdata %>%
      dplyr::filter(outcome == utils::tail(step_outcomes, 1)) %>%
      dplyr::select(arm, time) %>%
      unique() %>%
      dplyr::right_join(tmp, by = "arm")
  }

  plot <- plot +
    ggplot2::geom_segment(
      data = tmp,
      aes(x = x, y = y, xend = xend, yend = y,
          color = arm)
    )

  return(plot)
}

.add_boxplot <- function(plot, boxplot_data, w, add_v_lines = FALSE) {

  plot <- plot +
    ggplot2::geom_boxplot(data = boxplot_data$stats,
                          mapping = aes(xmin = xmin, xlower = xlower,
                                        xmiddle = xmiddle, xupper = xupper,
                                        xmax = xmax, y = y, colour = arm,
                                        fill = arm),
                          width = w, alpha = 0.5, stat = "identity",
                          orientation = "y")

  if (add_v_lines) {
    plot <- plot + ggplot2::geom_segment(
      data = boxplot_data$stats,
      mapping = aes(x = xmin,
                    y = y - (w / 0.75) * 0.1, yend = y + (w / 0.75) * 0.1,
                    colour = arm)
    ) +
      ggplot2::geom_segment(
        data = boxplot_data$stats,
        mapping = aes(x = xmax,
                      y = y - (w / 0.75) * 0.1, yend = y + (w / 0.75) * 0.1,
                      colour = arm)
      )
  }

  if (!is.null(boxplot_data$outlier)) {
    plot <- plot +
      ggplot2::geom_point(mapping = aes(x = x, y = y,
                                        colour = arm, fill = arm),
                          data = boxplot_data$outlier)
  }

  return(plot)
}

.add_labels_to_plot <- function(plot, minor_grid, minor_grid_x, scale, range,
                                start_last_endpoint, trans, obj, meta) {

  labels <- lapply(
    minor_grid,
    function(x) {
      s <- ifelse(scale > 0, 0, scale)
      return(as.character(round(x, -s + 1)))
    }
  )

  m_breaks <- .to_rangeab(
    minor_grid_x,
    start_last_endpoint,
    range[1],
    range[2]
  )

  if (trans == "reverse") {
    m_breaks <- start_last_endpoint - m_breaks + 100
  }

  plot <- plot +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = c(meta$proportion / 2 + meta$startx + 0.1),
      labels = c(obj$step_outcomes, obj$last_outcome),
      minor_breaks = m_breaks
    ) +
    ggplot2::annotate(
      geom = "text",
      x = m_breaks,
      y = 0,
      label = labels,
      color = "grey60"
    )

  return(plot)
}

.add_theme_to_plot <- function(plot, theme) {
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

  return(plot)
}
