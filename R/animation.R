#' Creates and returns an animated version of the standard maraca plot.
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
#' @param anim_order In what order should the treatment arms be animated?
#'                   Possible values are "active" (active arm is being animated
#'                   first), "control" (control arm is being animated first) or
#'                   "both" (both arms are being animated at the same time).
#'                   Default is "both".
#' @param gif_output Flag indicating if output should be rendered as gif.
#'                   Note that the 'gifski' package need to be installed in
#'                   order to create a gif output. Alternatively, if either the
#'                   'av' or 'ffmpeg' package is installed, a video file is
#'                   created. As a final alternative if no dependency is
#'                   installed, a list of image files is returned.
#' @param gif_file_name If gif output should be saved in file, provide file
#'                      name. If not provided (NULL), a tmp file will be created
#'                      to display gif.
#' @param frames_per_step The frame rate of the animation in frames/sec. Default
#'                        is 10.
#' @param gif_duration The length of the animation in seconds. Default is 10.
#' @param end_duration The amount of frames the animation pauses on the last
#'                     frame of the animation. Default is 20.
#' @param speed_factor This factor tries to balance the times that it takes to
#'                     animate the step outcomes towards the last outcome. If
#'                     not provided (NULL), the function tries to estimate a
#'                     number that approximately leads to a similar speed
#'                     between them. The user can try to experiment with
#'                     manually setting the number to get the best speed.
#' @param anim_width Width (in pixels) of the animation. Default value is 700.
#' @param anim_height Height (in pixels) of the animation. Default value is 500.
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
#' @return Depending on which dependencies are installed, a gif, video or
#'         list of image files are returned.
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
#' animation <- animate_plot(hce_test,
#'                           anim_order = "control",
#'                           gif_duration = 20)
#' @export
animate_plot <- function(obj, ...) {
  UseMethod("animate_plot", obj)
}

#' @rdname animate_plot
#' @export
animate_plot.default <- function(obj,
                                 ...) {
  paste0("animate_plot() function can only handle inputs of class ",
         "'adhce' or 'maraca'. Your input has class ", class(obj), ".")
}

#' @rdname animate_plot
#' @export
animate_plot.maraca <- function(
  obj,
  continuous_grid_spacing_x = NULL,
  trans = c("identity", "log", "log10", "sqrt", "reverse")[1],
  density_plot_type = c("default", "violin", "box", "scatter")[1],
  vline_type = NULL,
  remove_outliers = FALSE,
  theme = "maraca",
  anim_order = c("active", "control", "both")[3],
  gif_output = c(TRUE, FALSE)[1],
  gif_file_name = NULL,
  frames_per_step = 10,
  gif_duration = 10,
  end_duration = 20,
  speed_factor = NULL,
  anim_width = 700,
  anim_height = 500
) {

  if (!requireNamespace("gganimate", quietly = TRUE)) {
    stop(paste("Package \"gganimate\" must be installed to use animation",
               "functionality."))
  }

  checkmate::assert_flag(gif_output)

  if (gif_output && !requireNamespace("gifski", quietly = TRUE)) {
    stop(paste("Package \"gifski\" must be installed to save animation",
               "as gif."))
  }

  checkmate::assert_choice(anim_order,
                           choices = c("active", "control", "both"))

  checkmate::assert_numeric(gif_duration)
  checkmate::assert_numeric(speed_factor, null.ok = TRUE)
  checkmate::assert_string(gif_file_name, null.ok = TRUE)

  if (!is.null(gif_file_name)) {
    if (endsWith(gif_file_name, ".gif")) {
      fn <- gif_file_name
    } else {
      fn <- paste0(gif_file_name, ".gif")
    }
  }

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

  if (is.null(speed_factor)) {
    speed_factor <- ceiling(nrow(plotdata_last) / nrow(plotdata_ecdf))
  }

  plotdata_ecdf <- .step_outcomes_time_animation(plotdata_ecdf, obj,
                                                 step_outcomes,
                                                 step_types, speed_factor,
                                                 anim_order)

  idx <- plotdata_ecdf$arm == obj$arm_levels["control"]
  idx2 <- plotdata_last$arm == obj$arm_levels["control"]

  if (anim_order == "both") {
    plotdata_last$time <- rank(plotdata_last$x)
  } else {
    plotdata_last$time <- 1
    plotdata_last[idx2, ]$time <- rank(plotdata_last[idx2, ]$x)
    plotdata_last[!idx2, ]$time <- rank(plotdata_last[!idx2, ]$x)
  }

  active_time_ecdf <- plotdata_ecdf %>%
    dplyr::filter(arm == obj$arm_levels["active"]) %>%
    dplyr::pull(time) %>%
    max()
  control_time_ecdf <- plotdata_ecdf %>%
    dplyr::filter(arm == obj$arm_levels["control"]) %>%
    dplyr::pull(time) %>%
    max()

  plotdata_ecdf <- .animation_order_step(anim_order, plotdata_ecdf, idx, idx2,
                                         control_time_ecdf, active_time_ecdf)
  plotdata_last <- .animation_order_last(anim_order, plotdata_last, idx, idx2,
                                         control_time_ecdf, active_time_ecdf)

  if (density_plot_type == "box") {
    plot_min <- plotdata_last %>%
      dplyr::group_by(arm) %>%
      dplyr::summarise("time" = min(time) + 0.15 * min(time))
    plotdata_last <- dplyr::left_join(plotdata_last %>% dplyr::select(-time),
                                      plot_min, by = "arm")
  }

  if (last_type == "continuous" &&
        (density_plot_type %in% c("default", "box"))) {
    if (!is.null(boxplot_data$outlier)) {
      if (anim_order == "both") {
        boxplot_data$outlier$time <-
          max(plotdata_last$time) + 0.3 * max(plotdata_last$time)
      } else {
        boxplot_data$outlier$time <- 1
        boxplot_data$outlier[boxplot_data$outlier$arm ==
                               obj$arm_levels["control"], ]$time <-
          max(plotdata_last[plotdata_last$arm ==
                              obj$arm_levels["control"], ]$time) +
          0.1 * max(plotdata_last$time)
        boxplot_data$outlier[boxplot_data$outlier$arm ==
                               obj$arm_levels["active"], ]$time <-
          max(plotdata_last[plotdata_last$arm ==
                              obj$arm_levels["active"], ]$time) +
          0.1 * max(plotdata_last$time)
      }
    }
    if (anim_order == "both") {
      boxplot_data$stats$time <-
        max(plotdata_last$time) + 0.15 * max(plotdata_last$time)
    } else {
      boxplot_data$stats$time <- 1
      boxplot_data$stats[boxplot_data$stats$arm ==
                           obj$arm_levels["control"], ]$time <-
        max(plotdata_last[plotdata_last$arm ==
                            obj$arm_levels["control"], ]$time) +
        0.05 * max(plotdata_last$time)
      boxplot_data$stats[boxplot_data$stats$arm ==
                           obj$arm_levels["active"], ]$time <-
        max(plotdata_last[plotdata_last$arm ==
                            obj$arm_levels["active"], ]$time) +
        0.05 * max(plotdata_last$time)
    }
  }

  if ((last_type == "continuous" &&
         (density_plot_type %in% c("default", "violin"))) ||
        last_type == "binary") {

    plotdata_last <- .animation_polygon_data(plotdata_last, last_type)

  }

  if (last_type == "binary") {
    plotdata_last <- .animation_add_binary_point(plotdata_last, last_meta)
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
    plot <- .add_binary_steps_to_animation(plot, plotdata, step_outcomes,
                                           step_types, which_binary,
                                           speed_factor)
  }

  if (step_types[length(step_types)] == "binary") {
    plot <- .add_end_binary_step(plot, plotdata, step_outcomes,
                                 animation = TRUE)
  }

  width <- diff(range(plotdata_last$y))

  if (density_plot_type == "default") {
    if (last_type == "continuous") {
      plot <- .animation_polygon_plot(plot, plotdata,
                                      c("violin_lower", "violin_upper"))
      plot <- .add_boxplot(plot, boxplot_data, (width / 3),
                           add_v_lines = FALSE)
    } else if (last_type == "binary") {
      plot <- .animation_polygon_plot(plot, plotdata,
                                      c("binary_lower", "binary_upper"))
      plot <- plot +
        ggplot2::geom_point(data = plotdata[plotdata$type == "binary_point", ],
                            ggplot2::aes(x = value, y = y,
                                         color = arm))
    }
  } else if (density_plot_type == "violin") {
    plot <- .animation_polygon_plot(plot, plotdata,
                                    c("violin_lower", "violin_upper"))
  } else if (density_plot_type == "box") {
    plot <- .add_boxplot(plot, boxplot_data, (0.75 * width),
                         add_v_lines = TRUE)
  } else if (density_plot_type == "scatter") {
    # Jittering looks weird in the animation so instead using geom_point and
    # manually add a jitter
    # group = seq_along(time) - keeps the points from displayed one at a time
    n <- nrow(plotdata[plotdata$type == last_type, ])
    plot <- plot +
      ggplot2::geom_point(
        data = plotdata[plotdata$type == last_type, ],
        aes(x = x, y = y + runif(n, min = -width / 3, max = width / 3),
            color = arm, group = seq_along(time))
      )

  }

  plot <- .add_labels_to_plot(plot, minor_grid, minor_grid_x, scale, range,
                              start_last_endpoint, trans, obj, meta)

  if (!is.null(win_odds)) {
    plot <- .add_win_odds_to_plot(plot, win_odds, 0, Inf,
                                  hjust = 0, s = 3)
  }

  plot <- .add_theme_to_plot(plot, theme)

  plot <- plot +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 12)
    )

  anmt_plot <- plot +
    gganimate::transition_reveal(time)

  if (gif_output) {
    anmt_plot <- gganimate::animate(anmt_plot,
                                    renderer = gganimate::gifski_renderer(),
                                    end_pause = end_duration,
                                    fps = frames_per_step,
                                    duration = gif_duration,
                                    height = anim_height, width = anim_width,
                                    units = "px")
    if (!is.null(gif_file_name)) {
      gganimate::anim_save(fn, anmt_plot)
    }
  } else {
    anmt_plot <- gganimate::animate(anmt_plot,
                                    end_pause = end_duration,
                                    fps = frames_per_step,
                                    duration = gif_duration,
                                    height = anim_height, width = anim_width,
                                    units = "px")
  }

  return(anmt_plot)
}

#' @rdname animate_plot
#' @export
animate_plot.adhce <- function(x,
                               step_outcomes = NULL,
                               last_outcome = "C",
                               arm_levels = c(active = "A", control = "P"),
                               continuous_grid_spacing_x = NULL,
                               trans = c("identity", "log", "log10", "sqrt",
                                         "reverse")[1],
                               density_plot_type = c("default", "violin", "box",
                                                     "scatter")[1],
                               vline_type = NULL,
                               remove_outliers = FALSE,
                               theme = "maraca",
                               anim_order = c("active", "control", "both")[3],
                               gif_output = c(TRUE, FALSE)[1],
                               gif_file_name = NULL,
                               gif_duration = 10,
                               end_duration = 20,
                               speed_factor = NULL,
                               anim_width = 700,
                               anim_height = 500,
                               compute_win_odds = FALSE,
                               step_types = "tte",
                               last_type = "continuous",
                               lowerBetter = FALSE,
                               ...) {

  # Create maraca object
  maraca_dat <- .maraca_from_hce_data(x, step_outcomes,
                                      last_outcome, arm_levels,
                                      compute_win_odds = compute_win_odds,
                                      step_types = step_types,
                                      last_type = last_type,
                                      lowerBetter = lowerBetter)

  animated_plot <- animate_plot(maraca_dat,
                                continuous_grid_spacing_x =
                                  continuous_grid_spacing_x,
                                trans = trans,
                                density_plot_type = density_plot_type,
                                vline_type = vline_type,
                                remove_outliers = remove_outliers,
                                theme = theme,
                                anim_order = anim_order,
                                gif_output = gif_output,
                                gif_file_name = gif_file_name,
                                gif_duration = gif_duration,
                                end_duration = end_duration,
                                speed_factor = speed_factor,
                                anim_width = anim_width,
                                anim_height = anim_height)

  return(animated_plot)
}
