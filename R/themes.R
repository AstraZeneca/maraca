# Different themes that can be used to style the plots of the package

.theme_common <- function(p) {

  p <- p +
    ggplot2::xlab("Type of endpoint") +
    ggplot2::ylab("Cumulative proportion") +
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

.theme_color1 <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  colScheme <- c("deeppink4", "goldenrod1")
  names(colScheme) <- levels(p$data$arm)

  p <- p +
    ggplot2::scale_color_manual(name = "Arm", values = colScheme) +
    ggplot2::scale_fill_manual(values = colScheme)

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
