# Different themes that can be used to style the plots of the package

.theme_common <- function(p) {

  p <- p +
    ggplot2::xlab("Outcomes") +
    ggplot2::ylab("Cumulative percentage") +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    ) +
    ggplot2::guides(fill = "none")

  return(p)

}

.theme_common_cp <- function(p) {

  n <- length(levels(p$data$GROUP))
  p <- p +
    ggplot2::geom_vline(xintercept = seq(0.5, n + 1.5, 1),
                        linetype = 2, linewidth = 0.3, color = "darkgray") +
    # Axis showing percentages
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x, 2), "%"),
                                expand = ggplot2::expansion(mult = c(0, .3))) +
    ggplot2::ylab("Percent of all comparisons") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank())

  return(p)

}

.theme_maraca_cp <- function(p) {

  p <- .theme_common_cp(p)

  return(p)

}


.theme_maraca <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  return(p)

}

.theme_maraca_old <- function(p) {

  p <- .theme_common(p)

  p <- p +
    ggplot2::scale_color_discrete("Arm", labels = levels(p$data$arm)) +
    ggplot2::theme(
      axis.title.x.bottom =  ggplot2::element_blank()
    )

  return(p)

}

.theme_color1_cp <- function(p) {

  p <- .theme_common_cp(p)
  p <- p +
    ggplot2::scale_fill_manual(values = c("#830051", "#F0AB00",
                                          "#d3d3d3"), name = NULL)

  return(p)

}

.theme_color1 <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  colScheme <- c("#830051", "#F0AB00")
  names(colScheme) <- levels(p$data$arm)

  p <- p +
    ggplot2::scale_color_manual(name = "Arm", values = colScheme) +
    ggplot2::scale_fill_manual(values = colScheme)

  return(p)
}

.theme_color2_cp <- function(p) {

  p <- .theme_common_cp(p)
  p <- p +
    ggplot2::scale_fill_manual(values = c("#35B779FF", "#31688EFF",
                                          "#d3d3d3"), name = NULL)

  return(p)

}

.theme_color2 <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  colScheme <- c("#35B779FF", "#31688EFF")
  names(colScheme) <- levels(p$data$arm)

  p <- p +
    ggplot2::scale_color_manual(name = "Arm", values = colScheme) +
    ggplot2::scale_fill_manual(values = colScheme)

  return(p)
}


.theme_common_mosaic <- function(p) {

  p <- p +
    ggplot2::theme_light() + #just removes gridlines etc
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 10, angle = 45,
                                                       vjust = 1, hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 10),
                   axis.title.x =
                     ggplot2::element_text(size = 12,
                                           margin = ggplot2::margin(t = 20,
                                                                    r = 0,
                                                                    b = 0,
                                                                    l = 0)),
                   axis.title.y =
                     ggplot2::element_text(size = 12,
                                           margin = ggplot2::margin(t = 0,
                                                                    r = 20,
                                                                    b = 0,
                                                                    l = 0)),
                   axis.ticks.length = ggplot2::unit(20, "points"),
                   legend.text = ggplot2::element_text(size = 10),
                   legend.title = ggplot2::element_text(size = 10),
                   legend.position = "bottom",
                   plot.caption = element_text(hjust = 0.5, size = 10))

  return(p)
}

.theme_maraca_mosaic <- function(p) {

  p <- .theme_common_mosaic(p)

  return(p)

}

.theme_color1_mosaic <- function(p) {

  p <- .theme_common_mosaic(p)
  p <- p +
    ggplot2::scale_fill_manual(values = c("Loss" = "#830051",
                                          "Win" = "#F0AB00"),
                               name = "Results")

  return(p)

}

.theme_color2_mosaic <- function(p) {

  p <- .theme_common_mosaic(p)
  p <- p +
    ggplot2::scale_fill_manual(values = c("Loss" = "firebrick",
                                          "Win" = "#35B779FF"),
                               name = "Results")

  return(p)

}
