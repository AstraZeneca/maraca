
.create_validation_tte <- function(layers, x, arms) {

  tte_layers <- which(layers == "GeomStep")

  if (length(tte_layers) != 0) {
    tte_data <-
      do.call("rbind",
              lapply(tte_layers,
                     function(i) {
                       dat <- ggplot2::layer_data(plot = x,
                                                  i = i)[, c("x", "y",
                                                             "group")]
                       dat <- utils::head(dat, -2)
                       if (i == tte_layers[1]) {
                         dat <- utils::tail(dat, -2)
                       }
                       return(dat)
                     }))

    tte_data$group <- factor(tte_data$group, labels = arms)

  } else {
    tte_data <- NULL
  }

  return(tte_data)
}

.create_validation_binary_step <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  binary_layers <- which(layers == "GeomSegment")

  if (attr(x, "density_type") != "box" && length(binary_layers) != 0) {
    binary_step_data <-
      do.call("rbind",
              lapply(binary_layers,
                     function(i) {
                       dat <- ggplot2::layer_data(plot = x,
                                                  i = i)[, c("x", "y",
                                                             "yend",
                                                             "group",
                                                             "linetype")]
                       return(dat)
                     }))

    binary_step_data <- binary_step_data %>%
      dplyr::filter(linetype == 2) %>%
      dplyr::mutate(proportion = yend - y) %>%
      dplyr::select(x, y, proportion, group)

    binary_step_data$group <- factor(binary_step_data$group, labels = arms)

  } else {
    binary_step_data <- NULL
  }

  return(binary_step_data)
}

.create_validation_binary_last <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  polygon_layers <- which(layers == "GeomPolygon")
  point_layers <- which(layers == "GeomPoint")

  if (length(polygon_layers) == 1 &&
        length(point_layers) == 1 &&
        !("violin_scaling_factor" %in% names(attributes(x)))) {

    point_data <- ggplot2::layer_data(x, point_layers) %>%
      dplyr::select(x, y, group)

    polygon_data <- unique(ggplot2::layer_data(x, polygon_layers))
    polygon_data <- polygon_data %>%
      dplyr::filter(y %in% point_data$y) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise("lower_ci" = base::min(x, na.rm = TRUE),
                       "upper_ci" = base::max(x, na.rm = TRUE))

    binary_data <- dplyr::left_join(point_data, polygon_data,
                                    by = "group")
    binary_data$group <- factor(binary_data$group, labels = arms)

  } else {

    binary_data <- NULL

  }

  return(binary_data)
}



.create_validation_scatter <- function(layers, x, arms) {
  scatter_data <- do.call("rbind", lapply(which(layers == "GeomPoint"),
                                          ggplot2::layer_data, plot = x))
  if (attr(x, "density_type") == "scatter" && !is.null(scatter_data) &&
        nrow(scatter_data) > 2) {
    scatter_data <- scatter_data[, c("group", "x", "y")]
    scatter_data$group <- factor(scatter_data$group, labels = arms)
  } else {
    scatter_data <- NULL
  }

  return(scatter_data)
}

.create_validation_violin <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  violin_layer <- do.call("rbind", lapply(which(layers == "GeomPolygon"),
                                          ggplot2::layer_data, plot = x))

  if (!is.null(violin_layer) &&
        "violin_scaling_factor" %in% names(attributes(x))) {
    scaling_factor <- attr(x, "violin_scaling_factor")
    violin_layer$violinwidth <- violin_layer$y
    violin_help <- violin_layer %>%
      dplyr::group_by(group) %>%
      dplyr::summarize("y" = mean(violinwidth),
                       "width" = 1 * max(abs(violinwidth - y)))
    violin_help$width <- 2 * max(violin_help$width)
    violin_data <- violin_layer[, c("group", "x", "violinwidth")]
    violin_data <- dplyr::left_join(violin_data, violin_help)

    violin_data$density_scaled <- (violin_data$violinwidth - violin_data$y) /
      (violin_data$width / 2)

    violin_data$density <- violin_data$density_scaled * scaling_factor

    violin_data <- violin_data %>% dplyr::select(group, x, y, density,
                                                 density_scaled, violinwidth,
                                                 width)
    violin_data$group <- factor(violin_data$group, labels = arms)
  } else {
    violin_data <- NULL
  }

  return(violin_data)
}

.create_validation_box <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  boxstat_data <- do.call("rbind", lapply(which(layers == "GeomBoxplot"),
                                          ggplot2::layer_data, plot = x))

  if (!is.null(boxstat_data)) {
    boxstat_data <- boxstat_data %>%
      dplyr::select(group,
        "whisker_lower" = xmin,
        "hinge_lower" = xlower, "median" = xmiddle,
        "hinge_upper" = xupper, "whisker_upper" = xmax
      )
    outlier_data <- do.call("rbind", lapply(which(layers == "GeomPoint"),
                                            ggplot2::layer_data, plot = x))
    if (!is.null(outlier_data)) {
      outlier_data <- outlier_data %>%
        dplyr::group_by(group) %>%
        dplyr::summarise("outliers" = list(sort(x)),
                         "x_lowest" = min(x),
                         "x_highest" = max(x))
      boxstat_data <- dplyr::left_join(boxstat_data, outlier_data)
      boxstat_data$x_lowest <- pmin(boxstat_data$x_lowest,
                                    boxstat_data$whisker_lower)
      boxstat_data$x_highest <- pmax(boxstat_data$x_highest,
                                     boxstat_data$whisker_upper)
    } else {
      boxstat_data$outliers <- c()
    }
    boxstat_data$group <- factor(boxstat_data$group, labels = arms)
  }

  return(boxstat_data)
}
