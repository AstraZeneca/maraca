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
    dplyr::summarise(max_separation = max(separation)) %>%
    dplyr::ungroup()

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
    ) %>%
    dplyr::ungroup()

  meta2 <- hce_dat  %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(outcome, arm) %>%
    dplyr::summarise(n = n(), proportion = n / dim(hce_dat)[[1]] * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("arm" = gsub(" ", "_", tolower(arm))) %>%
    tidyr::pivot_wider(names_from = arm, values_from = c(n, proportion),
                       values_fill = 0)

  meta_missing <- hce_dat %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      missing = sum(is.na(value))
    ) %>%
    dplyr::ungroup()

  meta <- dplyr::left_join(meta1, meta2, "outcome")
  meta <- dplyr::left_join(meta, meta_missing, "outcome")

  return(meta)
}

# Calculates the cumulative distribution for TTE outcomes
.compute_ecdf_by_outcome <- function(
  hce_dat, meta, step_outcomes, step_types,
  last_outcome, arm_levels, fixed_followup_days
) {

  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  # Calculate the number of unique step outcomes
  num_step_outcomes <- length(step_outcomes)

  # Vectorize fixed follow-up days if there is only one value provided
  # For binary values, follow-up time is always 2 (to create a jump
  # in the middle of the step at 1)
  if (length(fixed_followup_days) == 1) {
    fixed_followup_days <- sapply(step_types, function(type) {
      ifelse(type == "binary", 2, fixed_followup_days)
    }, USE.NAMES = FALSE)
  }

  # Every step outcome will be plotted over an x-axis range from 0
  # to the fixed_follow_up days associated with the outcome

  # Initialize the cumulative time-to-event (t_cdf) with a maximum
  # value that is higher than the sum of all x-axis range parts
  # The reason for this is that when fitting the individual step
  # parts, we will go chronological and want to make sure that all
  # steps at a later stage have been initialized with a later time
  hce_dat$t_cdf <- sum(fixed_followup_days) + 2 * max(fixed_followup_days)
  # Initialize step_values column recording the size of the step (percentage
  # of number t risk) at each time point
  hce_dat$step_values <- 0

  # Iterate over each step outcome
  for (i in seq_len(num_step_outcomes)) {

    # Filter rows by outcome
    idx <- hce_dat$outcome == step_outcomes[[i]]
    # By default the value recorded in the data is the actual time of the step.
    # Since we add concatenate different step functions, we need to update the
    # x-axis to reflect the time of the step plus the cumulation of all the
    # x-axis ranges of the previous step functions
    add_previous_end <- ifelse(i == 1, 0, sum(fixed_followup_days[1:(i - 1)]))
    hce_dat[idx, ]$t_cdf <- hce_dat[idx, ]$value + add_previous_end

    # Iterate over each treatment arm
    for (arm in arm_levels) {
      # Filter rows by outcome and arm
      idx <- hce_dat$outcome == step_outcomes[[i]] & hce_dat$arm == arm

      # Fit the ECDF to the above updated x-axis range for the
      # cumulative time-to-event by treatment arm
      hce_dat[idx, ]$step_values <-
        100 *
        stats::ecdf(hce_dat[hce_dat$arm == arm,
                    ]$t_cdf)(hce_dat[idx, ]$t_cdf)

    }

  }

  hce_ecdf <- hce_dat %>%
    dplyr::filter(outcome %in% step_outcomes) %>%
    unique()

  # Double-check that all combinations of treatment and outcome have
  # been included (not the case if one combination has no patients)
  poss_comb <- expand.grid("outcome" = step_outcomes,
                           "arm" = arm_levels)
  missing_row <- dplyr::anti_join(poss_comb,
                                  hce_ecdf[, c("outcome", "arm")])

  # If there are missing rows, fill them in
  if (nrow(missing_row) > 0) {

    for (i in 1:num_step_outcomes) {
      # Check if current step outcome is missing
      if (step_outcomes[[i]] %in% missing_row$outcome) {
        tmp <- missing_row[missing_row$outcome == step_outcomes[[i]], ]
        # Determine step values based on previous step if available
        if (i == 1) {
          step_values <- 0
        } else {
          tmp2 <-  hce_ecdf[hce_ecdf$outcome == step_outcomes[i - 1] &
                              hce_ecdf$arm == tmp$arm, ]
          step_values <- max(tmp2$step_values)
        }
        # Fetch existing data for the same outcome but different arm
        tmp3 <-  hce_ecdf[hce_ecdf$outcome == step_outcomes[[i]] &
                            hce_ecdf$arm != tmp$arm, ]
        # Append missing row to the main data frame
        hce_ecdf <-
          rbind(hce_ecdf,
                data.frame(outcome = step_outcomes[[i]],
                           arm = tmp$arm,
                           t_cdf = mean(tmp3$t_cdf),
                           step_values = step_values,
                           value = 0))
      }
    }
  }

  # Order the data frame by step values
  hce_ecdf <- hce_ecdf[order(hce_ecdf$step_values), ]

  # Add names of set outcomes and associated type (tte or binary) to data
  endpoint <- data.frame("outcome" = step_outcomes,
                         "type" = step_types)
  hce_ecdf <- dplyr::left_join(hce_ecdf, endpoint,
                               by = "outcome")

  hce_ecdf$adjusted.time <- 0
  for (i in seq_len(num_step_outcomes)) {
    entry <- step_outcomes[i]
    outcome_filter <- hce_ecdf$outcome == entry
    hce_ecdf[outcome_filter, ]$adjusted.time <-
      meta[meta$outcome == entry, ]$startx +
      hce_ecdf[outcome_filter, ]$value /
      fixed_followup_days[i] *
      meta[meta$outcome == entry, ]$proportion
  }

  # Summarize maximum step values, type, and sum of events
  hce_ecdf_meta <- hce_ecdf %>%
    dplyr::group_by(arm, outcome) %>%
    dplyr::summarise(max = max(step_values, na.rm = TRUE),
      type = unique(type),
      sum.event = ifelse(type == "tte", n(),
                         unique(t_cdf))
    ) %>%
    dplyr::arrange(max) %>%
    dplyr::mutate(
      ecdf_end = utils::tail(max, 1)
    ) %>%
    dplyr::ungroup()

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
    hce_dat, meta, ecdf_mod, step_outcomes, last_outcome, arm_levels) {
  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  ctrl <- unname(arm_levels["control"])

  continuous_data <- hce_dat[hce_dat$outcome == last_outcome, ]
  start_continuous_endpoint <- meta[meta$outcome == last_outcome, ]$startx

  continuous_data$x <- .to_rangeab(
    continuous_data$value,
    start_continuous_endpoint,
    min(continuous_data$value, na.rm = TRUE),
    max(continuous_data$value, na.rm = TRUE)
  )
  continuous_meta <- continuous_data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(n = n(), median = stats::median(x, na.rm = TRUE),
                     average = base::mean(x, na.rm = TRUE)) %>%
    dplyr::ungroup()

  continuous_data$y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == unname(arm_levels["active"]) &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end
  continuous_data[continuous_data$arm == ctrl, ]$y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == ctrl &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end

  return(list(
    data = continuous_data,
    meta = continuous_meta
  ))
}

# Computes the binary information
.compute_binary <- function(
    hce_dat, meta, ecdf_mod, step_outcomes, last_outcome, arm_levels) {

  `%>%` <- dplyr::`%>%`
  n <- dplyr::n

  actv <- unname(arm_levels["active"])
  ctrl <- unname(arm_levels["control"])

  binary_data <- hce_dat[hce_dat$outcome == last_outcome, ]
  start_binary_endpoint <- meta[meta$outcome == last_outcome, ]$startx

  actv_y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == actv &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end
  ctrl_y <- ecdf_mod$meta[
    ecdf_mod$meta$arm == ctrl &
      ecdf_mod$meta$outcome == utils::tail(step_outcomes, 1),
  ]$ecdf_end

  binary_meta <- binary_data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarise(n = n(),
                     average = base::mean(value, na.rm = TRUE),
                     conf_int = 1.96 * sqrt((average * (1 - average)) / n)) %>%
    dplyr::ungroup()

  x_radius <- (100 - start_binary_endpoint) * min(binary_meta$conf_int)
  y_height <- min(c(0.4 * abs(actv_y - ctrl_y), 0.8 * x_radius))

  actv_point <-
    .create_ellipsis_points(unlist(binary_meta[binary_meta$arm == actv,
                                               "average"]),
                            actv_y,
                            unlist(binary_meta[binary_meta$arm == actv,
                                               "conf_int"]),
                            y_height)

  ctrl_point <-
    .create_ellipsis_points(unlist(binary_meta[binary_meta$arm == ctrl,
                                               "average"]),
                            ctrl_y,
                            unlist(binary_meta[binary_meta$arm == ctrl,
                                               "conf_int"]),
                            y_height)

  binary_data <- rbind(data.frame("outcome" = last_outcome,
                                  "arm" = actv,
                                  actv_point),
    data.frame("outcome" = last_outcome,
               "arm" = ctrl,
               ctrl_point)
  )

  binary_data$x <- .to_rangeab(
    binary_data$x,
    start_binary_endpoint,
    0,
    1
  )

  binary_meta$average <- .to_rangeab(
    binary_meta$average,
    start_binary_endpoint,
    0,
    1
  )

  binary_meta$y <- 0
  binary_meta[binary_meta$arm == actv, "y"] <- actv_y
  binary_meta[binary_meta$arm == ctrl, "y"] <- ctrl_y

  return(list(
    data = binary_data,
    meta = binary_meta
  ))
}

.create_ellipsis_points <- function(x0, y0, a, b) {

  points <- seq(0, 2 * pi, length.out = 361)
  cos_p <- cos(points)
  sin_p <- sin(points)
  x_tmp <- abs(cos_p) * a * sign(cos_p)
  y_tmp <- abs(sin_p) * b * sign(sin_p)
  edata <- data.frame(x = x0 + x_tmp, y = y0 + y_tmp)

  return(edata)

}

# Reformats the data coming in from outside so that it fits our expectation.
.reformat_and_check_data <- function(
    data, step_outcomes, last_outcome, arm_levels, column_names) {
  `%>%` <- dplyr::`%>%`
  vars <- dplyr::vars
  all_of <- dplyr::all_of

  hce_dat <- data %>%
    dplyr::rename(all_of(column_names)) %>%
    dplyr::select(all_of(names(column_names)))

  # Make sure outcome and arm columns are not factors
  hce_dat$outcome <- as.character(hce_dat$outcome)
  hce_dat$arm <- as.character(hce_dat$arm)

  endpoints <- c(step_outcomes, last_outcome)

  if (!all(as.character(unique(hce_dat[, "arm"])) %in%
             unname(arm_levels))) {
    stop(paste("Arm variable contains different values",
               "then given in parameter arm_levels"))
  }
  if (!all(as.character(unique(hce_dat[, "outcome"])) %in%
             unname(endpoints))) {
    stop(paste("Outcome variable contains different values",
               "then given in parameters step_outcomes and",
               "last_outcome"))
  }

  hce_dat <- hce_dat %>%
    dplyr::filter(outcome %in% endpoints) %>%
    dplyr::mutate_at(vars(outcome), factor, levels = endpoints) %>%
    dplyr::mutate_at(vars(arm), factor,
                     levels = c(arm_levels)[c("active", "control")])

  # Check if the endpoints are all present
  for (entry in c(step_outcomes, last_outcome)) {
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

.maraca_from_hce_data <- function(x, last_outcome, arm_levels,
                                  fixed_followup_days, compute_win_odds,
                                  step_types = "tte",
                                  last_type = "continuous") {

  checkmate::assert_string(last_outcome)
  checkmate::assert_names(names(x),
                          must.include = c("GROUP", "TRTP", "AVAL0"))

  checkmate::assert_names(
    names(arm_levels),
    permutation.of = c("active", "control")
  )

  checkmate::assert_flag(compute_win_odds)

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  tte <- sort(unique(x$GROUP)[unique(x$GROUP) != last_outcome])

  # Small bugfix to allow for name change of variable TTEFixed in newer
  # version of HCE package
  if ("PADY" %in% names(x)) {
    x$TTEfixed <- x$PADY
  }

  if (is.null(fixed_followup_days)) {
    checkmate::assertNames(names(x), must.include = "TTEfixed")
    checkmate::assert_integerish(x$TTEfixed)

    fixed_followup_days <- unname(sapply(tte, function(tte_ind) {
      x[x$GROUP == tte_ind, "TTEfixed"][[1]]
    }))
  }

  maraca_obj <- maraca(
    data = x,
    step_outcomes = tte,
    last_outcome = last_outcome,
    column_names = c(outcome = "GROUP", arm = "TRTP", value = "AVAL0"),
    arm_levels = arm_levels,
    fixed_followup_days = fixed_followup_days,
    compute_win_odds = compute_win_odds,
    step_types = step_types,
    last_type = last_type
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

.checks_continuous_outcome <- function(density_plot_type,
                                       vline_type) {
  checkmate::assert_choice(
    density_plot_type, c("default", "violin", "box", "scatter")
  )

  if (!(is.null(vline_type) ||
          checkmate::testString(vline_type))) {
    stop("vline_type has to be a string or NULL")
  }

  if (is.null(vline_type)) {
    vline_type <- "median"
  } else {
    checkmate::assert_choice(
      vline_type, c("median", "mean", "none")
    )
  }

  return(vline_type)
}

.checks_binary_outcome <- function(density_plot_type,
                                   vline_type) {
  checkmate::assert_choice(
    density_plot_type, c("default")
  )

  if (!(is.null(vline_type) ||
          checkmate::testString(vline_type))) {
    stop("vline_type has to be a string or NULL")
  }

  if (is.null(vline_type)) {
    vline_type <- "mean"
  } else {
    checkmate::assert_choice(
      vline_type, c("mean", "none")
    )
  }

  return(vline_type)
}

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

  binary_layers <- which(layers == "GeomSegment")

  if (length(binary_layers) != 0) {
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

  polygon_layers <- which(layers == "GeomPolygon")
  point_layers <- which(layers == "GeomPoint")

  if (length(polygon_layers) == 1 &&
        length(point_layers) == 1) {

    point_data <- ggplot2::layer_data(x, point_layers) %>%
      dplyr::select(x, y, group)

    polygon_data <- unique(ggplot2::layer_data(x, polygon_layers))
    polygon_data <- polygon_data %>%
      dplyr::filter(y %in% point_data$y) %>%
      dplyr::group_by(group) %>%
      dplyr::summarise("lower_se" = base::min(x, na.rm = TRUE),
                       "upper_se" = base::max(x, na.rm = TRUE))

    binary_data <- dplyr::left_join(point_data, polygon_data,
                                    by = "group")
    binary_data$se <- binary_data$x - binary_data$lower_se
    binary_data$group <- factor(binary_data$group, labels = arms)

  } else {

    binary_data <- NULL

  }

  return(binary_data)
}



.create_validation_scatter <- function(layers, x, arms) {
  scatter_data <- do.call("rbind", lapply(which(layers == "GeomPoint"),
                                          ggplot2::layer_data, plot = x))
  if (!is.null(scatter_data) && nrow(scatter_data) > 2) {
    scatter_data <- scatter_data[, c("group", "x", "y")]
    scatter_data$group <- factor(scatter_data$group, labels = arms)
  }

  return(scatter_data)
}

.create_validation_violin <- function(layers, x, arms) {
  violin_data <- do.call("rbind", lapply(which(layers == "GeomViolin"),
                                         ggplot2::layer_data, plot = x))
  if (!is.null(violin_data)) {
    violin_data <- violin_data[, c("group", "x", "y", "density", "width")]
    violin_data$group <- factor(violin_data$group, labels = arms)
  }

  return(violin_data)
}

.create_validation_box <- function(layers, x, arms) {

  `%>%` <- dplyr::`%>%`

  boxstat_data <- do.call("rbind", lapply(which(layers == "GeomBoxplot"),
                                          ggplot2::layer_data, plot = x))

  if (!is.null(boxstat_data)) {
    boxstat_data <- boxstat_data %>%
      dplyr::select(group, "x_lowest" = xmin_final,
                    "whisker_lower" = xmin,
                    "hinge_lower" = xlower, "median" = xmiddle,
                    "hinge_upper" = xupper, "whisker_upper" = xmax,
                    "x_highest" = xmax_final, outliers)
    boxstat_data$outliers <- lapply(boxstat_data$outliers, sort)
    boxstat_data$group <- factor(boxstat_data$group, labels = arms)
  }

  return(boxstat_data)
}
