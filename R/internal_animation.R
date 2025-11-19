.tte_time_animation <- function(o, df, arm_levels, speed_factor, anim_order) {
  df <- df[df$outcome == o, ]
  if (anim_order == "both") {
    df$time <- speed_factor * rank(df$x)
  } else {
    idx <- df$arm == arm_levels["control"]
    df$time <- 1
    df[idx, ]$time <- speed_factor * rank(df[idx, ]$x)
    df[!idx, ]$time <- speed_factor * rank(df[!idx, ]$x)
  }
  return(df)
}

.binary_time_animation <- function(o, df, speed_factor) {
  df <- df[df$outcome == o, ]
  df$time <- speed_factor * 5
  return(df)
}

.step_outcomes_time_animation <- function(df_ecdf, obj, step_outcomes,
                                          step_types, speed_factor,
                                          anim_order) {

  `%>%` <- dplyr::`%>%`

  df_ecdf <- do.call("rbind", lapply(seq_along(step_types), function(i) {
    df_time <- switch(step_types[i],
                      "tte" = .tte_time_animation(step_outcomes[i],
                                                  df_ecdf, obj$arm_levels,
                                                  speed_factor, anim_order),
                      "binary" = .binary_time_animation(step_outcomes[i],
                                                        df_ecdf,
                                                        speed_factor))
    return(df_time)
  }))

  for (i in 2:length(step_types)) {
    time_before <- df_ecdf %>%
      dplyr::filter(outcome == step_outcomes[i - 1]) %>%
      dplyr::group_by(arm) %>%
      dplyr::summarise("start_point" = ifelse(step_types[i - 1] == "tte",
                                              max(time),
                                              max(time) + 15 * speed_factor))
    df_ecdf[df_ecdf$outcome == step_outcomes[i], ]$time <-
      df_ecdf[df_ecdf$outcome == step_outcomes[i], ] %>%
      dplyr::left_join(time_before, by = "arm") %>%
      dplyr::mutate("time" = time + start_point) %>%
      dplyr::pull(time)
  }

  return(df_ecdf)
}

.animation_order_step <- function(anim_order, plotdata_ecdf,
                                  idx, idx2, control_time_ecdf,
                                  active_time_ecdf) {
  if (anim_order == "active") {
    plotdata_ecdf[idx, ]$time <- plotdata_ecdf[idx, ]$time +
      active_time_ecdf + sum(!idx2) + 1
  } else if (anim_order == "control") {
    plotdata_ecdf[!idx, ]$time <- plotdata_ecdf[!idx, ]$time +
      control_time_ecdf + sum(idx2) + 1
  }
  return(plotdata_ecdf)
}

.animation_order_last <- function(anim_order, plotdata_last,
                                  idx, idx2, control_time_ecdf,
                                  active_time_ecdf) {
  if (anim_order == "active") {
    plotdata_last[!idx2, ]$time <- plotdata_last[!idx2, ]$time +
      active_time_ecdf + 1
    plotdata_last[idx2, ]$time <- plotdata_last[idx2, ]$time +
      active_time_ecdf + control_time_ecdf + sum(!idx2) + 2
  } else if (anim_order == "control") {
    plotdata_last[idx2, ]$time <- plotdata_last[idx2, ]$time +
      control_time_ecdf + 1
    plotdata_last[!idx2, ]$time <- plotdata_last[!idx2, ]$time +
      active_time_ecdf + control_time_ecdf + sum(idx2) + 2
  } else if (anim_order == "both") {
    plotdata_last$time <- plotdata_last$time +
      max(active_time_ecdf, control_time_ecdf) + 1
  }

  return(plotdata_last)
}

.animation_add_binary_point <- function(plotdata_last, last_meta) {
  `%>%` <- dplyr::`%>%`

  add_binary_point <- data.frame("outcome" = unique(plotdata_last$outcome),
                                 "arm" = unique(plotdata_last$arm),
                                 "type" = "binary_point")
  add_binary_point <- dplyr::left_join(add_binary_point,
                                       (last_meta %>%
                                          dplyr::select(arm, x, y,
                                                        "value" = average)),
                                       by = "arm")
  time_point <- plotdata_last %>%
    dplyr::group_by(arm) %>%
    dplyr::summarize("time" = max(time) + 1)
  add_binary_point <- dplyr::left_join(add_binary_point, time_point,
                                       by = "arm")

  plotdata_last <- rbind(plotdata_last, add_binary_point)

  return(plotdata_last)
}

.animation_polygon_data <- function(plotdata_last, last_type) {

  `%>%` <- dplyr::`%>%`

  plotdata_last_stats <- plotdata_last %>%
    dplyr::group_by(arm) %>%
    dplyr::summarize("mean" = mean(y))

  plotdata_last <- dplyr::left_join(plotdata_last, plotdata_last_stats,
                                    by = "arm")

  if (last_type == "binary") {
    plotdata_last$type <- ifelse(plotdata_last$y > plotdata_last$mean,
                                 "binary_upper", "binary_lower")
  } else {
    plotdata_last$type <- ifelse(plotdata_last$y > plotdata_last$mean,
                                 "violin_upper", "violin_lower")
  }
  plotdata_last <- plotdata_last %>% dplyr::select(-mean)

  return(plotdata_last)
}

.animation_polygon_plot <- function(plot, df, polygon_type) {
  plot <- plot +
    ggplot2::geom_line(mapping = aes(x, y, group = arm,
                                     colour = arm),
                       data = df[df$type %in% polygon_type, ],
                       alpha = 0.5) +
    ggplot2::geom_line(mapping = aes(x, y, group = arm,
                                     colour = arm),
                       data = df[df$type == polygon_type[1], ]) +
    ggplot2::geom_line(mapping = aes(x, y, group = arm,
                                     colour = arm),
                       data = df[df$type == polygon_type[2], ])
  return(plot)
}
