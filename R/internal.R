### internal functions

# For printing - upper case first letter of each word
.title_case <- function(x) {
  sapply(x, function(word) {
    word_split <- strsplit(word, " ")
    paste(sapply(word_split, function(w) {
      paste0(toupper(substring(w, 1, 1)),
             tolower(substring(w, 2, nchar(w))))
    }), collapse = " ")
  })
}

# Computes the win odds from the internal data.
.compute_win_odds <- function(hce_dat, arm_levels) {
  hce_dat <- base::as.data.frame(hce_dat)
  hce_dat <- .with_ordered_column(hce_dat)
  fit <- hce::calcWO(x = hce_dat, AVAL = "ordered",
                     TRTP = "arm",
                     ref = unname(arm_levels["control"]))
  ci <- base::as.numeric(fit[, base::c("WO", "LCL", "UCL")])
  p <- fit$Pvalue
  win_odds <- base::c(ci, p)
  names(win_odds) <- base::c("estimate", "lower", "upper", "p-value")

  win_odds_outcome <- hce::summaryWO(hce_dat, AVAL = "ordered", TRTP = "arm",
                                     ref = unname(arm_levels["control"]),
                                     GROUP = "outcome")

  return(list("win_odds" = win_odds,
              "win_odds_outcome" = win_odds_outcome))

}

# This function does a bit of dirty magic to distribute the values
# onto different "floors", each floor being a numeric offset that is higher
# for each passing tte variable (and highest for the continuous).
# In practice, we are translating the values for each tte variable group.
# Explanation inline
.with_ordered_column <- function(hce_dat) {
  # We create a data frame, grouping according to the outcome,
  # then we get the minimum and maximum values of the value.
  # What we want to know is the "window" where data are for each of the groups
  # We then select the largest window.
  `%>%` <- dplyr::`%>%`

  tmp <- hce_dat %>%
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
  hce_dat <- hce_dat %>%
    dplyr::mutate(ordered = .env$gap * (as.numeric(outcome) - 1) + value)

  # and now we have a new data set with the column added.
  return(hce_dat)
}

# Computes the metainfo from the internal HCE data.
.compute_metainfo <- function(hce_dat) {
  n <- dplyr::n
  `%>%` <- dplyr::`%>%`

  meta1 <- hce_dat  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      n = n(),
      proportion = n / dim(hce_dat)[[1]] * 100,
      maxday = max(value, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      startx = c(0, cumsum(utils::head(proportion, -1))),
      endx = cumsum(proportion),
      starty = 0,
      n.groups = length(unique(outcome))
    )

  meta2 <- hce_dat  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome, arm) %>%
    dplyr::summarise(n = n(), proportion = n / dim(hce_dat)[[1]] * 100) %>%
    dplyr::mutate("arm" = gsub(" ", "_", tolower(arm))) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(n, proportion))

  meta_missing <- hce_dat %>%
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
  hce_dat, meta, tte_outcomes, continuous_outcome, arm_levels,
  fixed_followup_days
) {

  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  num_tte_outcomes <- length(tte_outcomes)
  hce_dat$t_cdf <- (num_tte_outcomes + 2) * fixed_followup_days

  for (i in seq_len(num_tte_outcomes)) {
    hce_dat[hce_dat$outcome == tte_outcomes[[i]], ]$t_cdf <-
      hce_dat[hce_dat$outcome == tte_outcomes[[i]], ]$value +
      fixed_followup_days * (i - 1)
  }

  hce_ecdf <-
    do.call("rbind",
            lapply(unique(hce_dat$arm), function(a, df, outcomes) {
              tmp <- df %>% dplyr::filter(arm == a)
              tmp$ecdf_values <- 100 *
                stats::ecdf(tmp$t_cdf)(tmp$t_cdf)
              tmp %>% dplyr::filter(outcome %in% outcomes)
            }, df = hce_dat, outcomes = tte_outcomes))

  hce_ecdf <- hce_ecdf[order(hce_ecdf$ecdf_values), ]

  hce_ecdf$adjusted.time <- 0
  for (entry in tte_outcomes) {
    outcome_filter <- hce_ecdf$outcome == entry
    hce_ecdf[outcome_filter, ]$adjusted.time <-
      meta[meta$outcome == entry, ]$startx +
      hce_ecdf[outcome_filter, ]$value /
      fixed_followup_days *
      meta[meta$outcome == entry, ]$proportion
  }

  hce_ecdf_meta <- hce_ecdf %>%
    dplyr::group_by(arm, outcome) %>%
    dplyr::summarise(max = max(ecdf_values, na.rm = TRUE),
                     sum.event = n()) %>%
    dplyr::mutate(
      ecdf_end = utils::tail(max, 1)
    )

  return(list(
    data = hce_ecdf,
    meta = hce_ecdf_meta
  ))
}

# Support function for the range
.to_rangeab <- function(x, start_continuous_endpoint, minval, maxval) {
  (100 - start_continuous_endpoint) * (x - minval) /
    (maxval - minval) + start_continuous_endpoint
}

# Computes the continuous information
.compute_continuous <- function(
    hce_dat, meta, ecdf_mod, tte_outcomes, continuous_outcome, arm_levels) {
  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  ctrl <- unname(arm_levels["control"])

  continuous_data <- hce_dat[hce_dat$outcome == continuous_outcome, ]
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
    ecdf_mod$meta$arm == unname(arm_levels["active"]) &
      ecdf_mod$meta$outcome == utils::tail(tte_outcomes, 1),
  ]$ecdf_end
  continuous_data[continuous_data$arm == ctrl, ]$y_level <- ecdf_mod$meta[
    ecdf_mod$meta$arm == ctrl &
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

  hce_dat <- data %>%
    dplyr::rename(all_of(column_names)) %>%
    dplyr::select(all_of(names(column_names)))

  # Make sure outcome and arm columns are not factors
  hce_dat$outcome <- as.character(hce_dat$outcome)
  hce_dat$arm <- as.character(hce_dat$arm)

  endpoints <- c(tte_outcomes, continuous_outcome)

  if (!all(as.character(unique(hce_dat[, "arm"])) %in%
             unname(arm_levels))) {
    stop(paste("Arm variable contains different values",
               "then given in parameter arm_levels"))
  }
  if (!all(as.character(unique(hce_dat[, "outcome"])) %in%
             unname(endpoints))) {
    stop(paste("Outcome variable contains different values",
               "then given in parameters tte_outcomes and",
               "continuous_outcome"))
  }

  hce_dat <- hce_dat %>%
    dplyr::filter(outcome %in% endpoints) %>%
    dplyr::mutate_at(vars(outcome), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(arm), factor,
                     levels = c(arm_levels)[c("active", "control")])

  # Check if the endpoints are all present
  for (entry in c(tte_outcomes, continuous_outcome)) {
    if (!any(hce_dat$outcome == entry)) {
      stop(paste(
        "Outcome", entry, "is not present in column",
        column_names[["outcome"]]
      ))
    }
  }

  return(hce_dat)

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

.maraca_from_hce_data <- function(x, continuous_outcome, arm_levels,
                                  fixed_followup_days, compute_win_odds) {

  checkmate::assert_string(continuous_outcome)
  checkmate::assert_names(names(x),
                          must.include = c("GROUP", "TRTP", "AVAL0"))

  checkmate::assert_names(
    names(arm_levels),
    permutation.of = c("active", "control")
  )

  checkmate::assert_flag(compute_win_odds)

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  tte <- sort(unique(x$GROUP)[unique(x$GROUP) != continuous_outcome])

  # Small bugfix to allow for name change of variable TTEFixed in newer
  # version of HCE package
  if ("PADY" %in% names(x)) {
    x$TTEfixed <- x$PADY
  }

  if (is.null(fixed_followup_days)) {
    checkmate::assertNames(names(x), must.include = "TTEfixed")
    checkmate::assert_int(x$TTEfixed[[1]])
    fixed_followup_days <- x$TTEfixed[[1]]
  }

  maraca_obj <- maraca(
    data = x,
    tte_outcomes = tte,
    continuous_outcome = continuous_outcome,
    column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
    arm_levels = arm_levels,
    fixed_followup_days = fixed_followup_days,
    compute_win_odds = compute_win_odds
  )

  return(maraca_obj)
}


# Preparing dataset to be used for plotting components
.prep_data_component_plot <- function(win_odds_outcome, endpoints, arms) {

  `%>%` <- dplyr::`%>%`

  # Win odds summary for each outcome from maraca object
  wo_bar_nc <- win_odds_outcome$summary_by_GROUP

  # Add overall numbers
  wo_tot <- win_odds_outcome$summary
  wo_tot <- wo_tot %>%
    dplyr::mutate("GROUP" = "Overall") %>%
    dplyr::select(names(win_odds_outcome$summary_by_GROUP))

  wo_bar_nc <- rbind(wo_tot, wo_bar_nc)

  wo_bar_nc <- wo_bar_nc %>%
    # Order according to outcome
    dplyr::arrange(match(GROUP, endpoints)) %>%
    # Wide format to get 1 line per outcome
    tidyr::pivot_wider(names_from = TRTP,
                       values_from = c(WIN, LOSS, TIE, TOTAL)) %>%
    # Selecting variables of interest and renaming for plotting
    dplyr::select(GROUP, "A_wins" = WIN_A, "P_wins" = WIN_P,
                  "Ties" = TIE_A) %>%
    # Long format for plotting
    tidyr::pivot_longer(cols = c("A_wins", "P_wins", "Ties"),
                        names_to = "name", values_to = "value")

  # Total number of wins/losses/ties to get relative results
  wo_bar_nc$total <- wo_tot$TOTAL[1]

  # Calculate percentage results
  wo_bar_nc$percentage <- 100 * (wo_bar_nc$value / wo_bar_nc$total)

  labels <- c(paste(arms["active"], "wins"),
              paste(arms["control"], "wins"),
              "Ties")

  wo_bar_nc$name <- ifelse(wo_bar_nc$name == "A_wins",
                           labels[1],
                           ifelse(wo_bar_nc$name == "P_wins",
                                  labels[2], labels[3]))

  wo_bar_nc$name <- factor(wo_bar_nc$name, levels = labels)

  return(wo_bar_nc)
}


# The main plotting function creating the component plot
.create_component_plot <- function(wo_bar_nc, endpoints, theme) {

  aes <- ggplot2::aes

  wo_bar_nc$GROUP <- factor(wo_bar_nc$GROUP,
                            levels = rev(c("Overall", endpoints)))

  plot <-
    ggplot2::ggplot(data = wo_bar_nc, aes(x = GROUP, y = percentage,
                                          fill = name)) +
    # Bars
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      width = .8) +
    # Flip to show bars horizontally
    ggplot2::coord_flip() +
    # Add wins/losses/ties as labels
    ggplot2::geom_text(aes(label = round(percentage, 1)),
                       position = ggplot2::position_dodge(width = .8),
                       vjust = 0.5, hjust = -0.2)

  plot <- switch(theme,
                 "maraca" = .theme_maraca_cp(plot),
                 "color1" = .theme_color1_cp(plot),
                 "color2" = .theme_color2_cp(plot),
                 "none" = plot,
                 stop("Please provide theme that exists"))

  # Add class to plot - componentPlot
  class(plot) <- c("componentPlot", class(plot))

  return(plot)
}

.add_win_odds_to_plot <- function(p, win_odds, x, y, hjust) {

  p <- p +
    ggplot2::annotate(
      geom = "label",
      x = x,
      y = y,
      label = paste(
        "Win odds: ", round(win_odds[[1]], 2),
        "\n95% CI: ", round(win_odds[[2]], 2), " - ",
        round(win_odds[[3]], 2), "\n",
        "p-value: ", format.pval(win_odds[[4]], digits = 3, eps = 0.001),
        sep = ""
      ),
      hjust = hjust, vjust = 1.4, size = 3
    )

  return(p)
}
