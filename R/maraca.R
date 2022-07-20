library(ggfortify)



maraca <- function(filename) {

  `%>%` <- dplyr::`%>%`

  ##########################
  # Create the HCE dataset #
  ##########################

  ### Read in the data ###
  HCE <- read.csv(filename)

  vars <- dplyr::vars

  # Input parameters
  # Order of the endpoints. The continuous endpoint should always be last.
  endpoints <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV",
    "Continuous outcome")
  treatments <- c("Active", "Control")
  fixed_followup_days <- 3 * 365

  # Remove unwanted endpoints and treatments
  HCE <- HCE %>%
    dplyr::filter(GROUP %in% endpoints) %>%
    dplyr::mutate_at(vars(GROUP), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(TRTP), factor, levels = treatments)



  ##############################################
  # Function to calculate the Win-Odds results #
  ##############################################
  #
  # wo.result <- calcWinOdds(
    # data = HCE, ordinalVal = "AVAL", group = "TRTP", reference = "Control")


  grp <- sanon::grp
  fit <- sanon::sanon(AVAL ~ grp(TRTP, ref = "Control"), data = HCE)
  CI0 <- confint(fit)$ci
  CI <- CI0 / (1 - CI0)
  p <- fit$p

  win_odds <- c(CI, p)
  names(win_odds) <- c("estimate", "lower", "upper", "p-value")




  ##############################################
  ### Calculations that support the ploting  ###
  ##############################################


  # Calculate meta information from the entire HCE dataset needed for plotting

  meta <- .compute_metainfo(HCE, fixed_followup_days)
  survmod <- .compute_survmod(HCE, meta, endpoints, treatments)
  slope <- .compute_slope(HCE, meta, survmod, endpoints, treatments)

  return(
    structure(
      list(
        meta = meta,
        slope = slope,
        survmod = survmod,
        endpoints = endpoints,
        win_odds = win_odds
      ),
      class = c("maraca::maraca")
    )
  )
}

plot_maraca <- function(obj) {
  aes <- ggplot2::aes

  meta <- obj$meta
  slope <- obj$slope
  survmod <- obj$survmod
  endpoints <- obj$endpoints
  win_odds <- obj$win_odds
  start_continuous_endpoint <- meta[meta$GROUP == tail(endpoints, 1), ]$startx

  minor_grid <- seq(
    sign(min(slope$data$AVAL0)) * floor(abs(min(slope$data$AVAL0)) / 10) * 10,
    sign(max(slope$data$AVAL0)) * floor(abs(max(slope$data$AVAL0)) / 10) * 10,
    by = 10
  )

  zeroposition <- .to_rangeab(0,
    start_continuous_endpoint,
    min(slope$data$AVAL0),
    max(slope$data$AVAL0)
  )
  # Plot the information in the Maraca plot
  plot <- ggplot2::ggplot(survmod$data, aes(colour = TRTP)) +
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
      aes(x = violinx, y = violiny, color = TRTP)
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
      labels = endpoints,
      minor_breaks = .to_rangeab(
        minor_grid,
        start_continuous_endpoint,
        min(slope$data$AVAL0),
        max(slope$data$AVAL0)
      )
    ) +
    ggplot2::annotate(
      geom = "text",
      x = .to_rangeab(
        minor_grid,
        start_continuous_endpoint,
        min(slope$data$AVAL0),
        max(slope$data$AVAL0)
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

plot_tte_trellis <- function(obj) {
  survmod <- obj$survmod
  plot <- ggplot2::ggplot(survmod$data) +
    ggplot2::geom_line(aes(x = time, y = km.y * 100, color = strata)) +
    ggplot2::facet_grid(cols = vars(GROUP))
  return(plot)
}

`plot.maraca::maraca` <- function(obj) {
  print(plot_maraca(obj))
}

# Private
.win_odds <- function(data, ordinal_val, treatments, reference) {
  grp <- sanon::grp
  fit <- sanon::sanon(
    ordinal_val ~ grp(treatments, ref = reference), data = data
  )
  CI0 <- confint(fit)$ci
  CI <- CI0 / (1 - CI0)
  p <- fit$p

  wo.result <- c(CI, p)
  names(wo.result) <- c("estimate", "lower", "upper", "p-value")

  return(wo.result)
}

.compute_metainfo <- function(HCE, fixed_followup_days) {
  n <- dplyr::n

  meta1 <- HCE %>%
    dplyr::group_by(GROUP) %>%
    dplyr::summarise(
      n = n(), proportion = n / dim(HCE)[1] * 100, maxday = max(AVAL0)) %>%
    dplyr::mutate(
      fixed.followup = fixed_followup_days,
      startx = c(0, cumsum(head(proportion, -1))),
      endx = cumsum(proportion),
      starty = 0,
      n.groups = length(unique(GROUP))
    )

  meta2 <- HCE %>%
    dplyr::group_by(GROUP, TRTP) %>%
    dplyr::summarise(n = n(), proportion = n / dim(HCE)[1] * 100) %>%
    tidyr::pivot_wider(names_from = TRTP, values_from = c(n, proportion))

  meta <- dplyr::left_join(meta1, meta2, "GROUP")

  return(meta)
}

.compute_survmod <- function(HCE, meta, endpoints, treatments) {
  # Use the largest value across the hard endpoints if i
  # fixed.follow.up.days is not specified
  HCE$kmday <- max(meta[meta$GROUP %in% head(endpoints, -1), ]$maxday)
  # Use the specified length of the fixed-follow-up trial if specified
  HCE$kmday <- meta$fixed.followup[1]

  HCE[HCE$GROUP %in% head(endpoints, -1), ]$kmday <- HCE[
    HCE$GROUP %in% head(endpoints, -1), ]$AVAL0

  Surv <- survival::Surv
  # Create survival model dataset
  survmod_data <- cbind(
    ggplot2::fortify(with(HCE,
      survival::survfit(
        Surv(time = kmday, event = GROUP == endpoints[1]) ~ TRTP))
    ), GROUP = endpoints[1])

  for (i in 2:length(endpoints) - 1) {
    survmod_data <- rbind(
      survmod_data,
      cbind(
        ggplot2::fortify(with(HCE,
          survival::survfit(
            Surv(time = kmday, event = GROUP == endpoints[i]) ~ TRTP))
            ), GROUP = endpoints[i]))
  }

  survmod_data <- survmod_data %>%
    dplyr::mutate_at(vars(GROUP), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(strata), factor, levels = treatments)

  survmod_data$adjusted.time <- 0
  for (i in head(endpoints, -1)) {
    survmod_data[survmod_data$GROUP == i, ]$adjusted.time <- meta[
      meta$GROUP == i, ]$startx +
      survmod_data[survmod_data$GROUP == i, ]$time /
      max(survmod_data[survmod_data$GROUP == i, ]$time) *
      meta[meta$GROUP == i, ]$proportion
  }

  survmod_data <- survmod_data %>% dplyr::mutate(km.y = 1 - surv)

  survmod_meta <- survmod_data %>%
    dplyr::group_by(strata, GROUP) %>%
    dplyr::summarise(max = 100 * max(1 - surv), sum.event = sum(n.event)) %>%
    dplyr::mutate(km.start = c(0, cumsum(head(max, -1))),
      km.end = cumsum(max))

  survmod_data <- survmod_data %>% dplyr::left_join(
    survmod_meta, by = c("strata", "GROUP")
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

.compute_slope <- function(HCE, meta, survmod, endpoints, treatments) {
  n <- dplyr::n

  slope_data <- HCE[HCE$GROUP == tail(endpoints, 1), ]
  start_continuous_endpoint <- meta[meta$GROUP == tail(endpoints, 1), ]$startx

  slope_data$x <- .to_rangeab(
    slope_data$AVAL0,
    start_continuous_endpoint,
    min(slope_data$AVAL0),
    max(slope_data$AVAL0)
  )

  slope_meta <- slope_data %>%
    dplyr::group_by(TRTP) %>%
    dplyr::summarise(n = n(), median = median(x), average = mean(x))

  slope_data$violinx <- 0
  slope_data[slope_data$TRTP == treatments[1], ]$violinx <- seq(
    from = start_continuous_endpoint, to = 100,
    length.out = slope_meta$n[1])
  slope_data[slope_data$TRTP == treatments[2], ]$violinx <- seq(
    from = start_continuous_endpoint, to = 100,
    length.out = slope_meta$n[2])

  slope_data$violiny <- survmod$meta[
    survmod$meta$strata == treatments[1] &
    survmod$meta$GROUP == endpoints[length(endpoints) - 1],
    ]$km.end
  slope_data[slope_data$TRTP == treatments[2], ]$violiny <- survmod$meta[
    survmod$meta$strata == treatments[2] &
    survmod$meta$GROUP == endpoints[length(endpoints) - 1],
    ]$km.end

  return(list(
    data = slope_data,
    meta = slope_meta
  ))
}

# Function that scale data to a range
rangeab <- function(x, a, b) {
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a
}
