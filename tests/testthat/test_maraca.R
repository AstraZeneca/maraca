.maraca_args <- function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  return(list(
    data = data,
    tte_outcomes = tte_outcomes,
    continuous_outcome = continuous_outcome,
    arm_levels = arm_levels,
    column_names = column_names
  ))
}


test_that("Maraca initialisation", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)

  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  fixed_followup_days <- 3 * 365
  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels,
    column_names,
    fixed_followup_days
    )
  expect_s3_class(mar, "maraca")
  expect_equal(mar$fixed_followup_days, fixed_followup_days)
  plot(mar)
})

test_that("Maraca wrong params", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  expect_error(
    maraca(
      "hello", tte_outcomes, continuous_outcome, arm_levels,
      fixed_followup_days = fixed_followup_days
    ), regexp = "Must be of type 'data\\.frame'"
  )
  expect_error(
    maraca(
      data, c(1, 2, 3), continuous_outcome, arm_levels,
      fixed_followup_days = fixed_followup_days
    ), regexp = "Must be of type 'character'"

  )
  expect_error(
    maraca(data, tte_outcomes, 3, arm_levels,
      fixed_followup_days = fixed_followup_days
    ),
    regexp = "Must be of type 'string'"
  )
  expect_error(
    maraca(data, tte_outcomes, continuous_outcome, c(1, 2),
           fixed_followup_days = fixed_followup_days),
    regexp = "Must be of type 'character'"
  )
  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome,
      c(active = "foo", control = "bar", whatever = "baz"),
      column_names,
      fixed_followup_days), regexp = "Must have length 2"
  )
  expect_error(
    maraca(data, tte_outcomes, continuous_outcome, arm_levels,
           column_names,
           fixed_followup_days = 12.3
          ),
    regexp = "single integerish value"
  )
  expect_error(
    maraca(data, tte_outcomes, continuous_outcome, arm_levels,
           column_names,
           fixed_followup_days = NULL
    )
  )
  expect_error(
    maraca(data, tte_outcomes, continuous_outcome, arm_levels,
           column_names,
           fixed_followup_days = 12
    ),
    regexp = "Time-to-event data contain events after the fixed_followup_days"
  )

  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c("a"),
      fixed_followup_days
    ),
    regexp = "Must have length 3"
  )
  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c("a", "b", "c"),
      fixed_followup_days
    ),
    regexp = "Must have names"
  )
  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c(foo = "a", bar = "b", baz = "c"),
      fixed_followup_days
    ),
    regexp = "Names must be a identical to"
  )

  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c(
        outcome = "GROUP", arm = "notexistent",
        value = "AVAL0"
      ), fixed_followup_days
    ),
    regexp = "Can't rename columns that don't exist"
  )

  data2 <- data.frame(data)
  data2$TRTP <- as.factor(data2$TRTP)
  expect_error(
    maraca(
      data2, tte_outcomes, continuous_outcome, arm_levels,
      c(
        outcome = "GROUP", arm = "TRTP",
        value = "AVAL0"
      ), fixed_followup_days
    ),
    regexp = "The arm column must be characters"
  )

  data2 <- data.frame(data)
  data2$GROUP <- as.factor(data2$GROUP)
  expect_error(
    maraca(
      data2, tte_outcomes, continuous_outcome, arm_levels,
      c(
        outcome = "GROUP", arm = "TRTP",
        value = "AVAL0"
      ), fixed_followup_days
    ),
    regexp = "The outcome column must be characters"
  )
})

test_that("Maraca printing", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )
  data_rf <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
                                   arm_levels, column_names = column_names
  )
  win_odds <- .compute_win_odds(data_rf)

  mar_no_win_odds <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )
  data_rf <- .reformat_and_check_data(data[!is.na(data$AVAL0), ], tte_outcomes,
                                      continuous_outcome,
                                      arm_levels, column_names = column_names
  )
  win_odds_na <- .compute_win_odds(data_rf)

  expect_output(print(mar),
                "Maraca object for plotting maraca graph created")
  expect_output(print(mar), paste0(nrow(data)))

  expect_output(print(mar_no_win_odds),
                "Maraca object for plotting maraca graph created")
  expect_output(print(mar_no_win_odds), paste0(nrow(data)))

  expect_output(print(mar_na),
                "Maraca object for plotting maraca graph created")
  expect_output(print(mar_na), paste0(nrow(data) - 1))
  expect_output(print(mar_na),
                "1 patient\\(s\\) removed because of missing values.")

  expect_output(print(mar),
                "Maraca object for plotting maraca graph created")
  expect_output(print(mar), paste0(nrow(data)))

  expect_output(print(mar), paste(round(win_odds["estimate"], 2)))
  expect_output(print(mar), paste(round(win_odds["lower"], 2)))
  expect_output(print(mar), paste(round(win_odds["upper"], 2)))
  expect_output(print(mar),
                paste(format.pval(win_odds["p-value"],
                                  digits = 3, eps = 0.001)))

  expect_output(print(mar_na), paste(round(win_odds_na["estimate"], 2)))
  expect_output(print(mar_na), paste(round(win_odds_na["lower"], 2)))
  expect_output(print(mar_na), paste(round(win_odds_na["upper"], 2)))
  expect_output(print(mar_na),
                paste(format.pval(win_odds_na["p-value"],
                                  digits = 3, eps = 0.001)))

  expect_output(print(mar_no_win_odds), "Win odds not calculated.")

  expect_output(print(mar), paste(mar$meta$n[1]))
  expect_output(print(mar), paste(mar$meta$n[2]))
  expect_output(print(mar), paste(mar$meta$n[3]))
  expect_output(print(mar), paste(mar$meta$n[4]))

  expect_output(print(mar_na), paste(mar_na$meta$proportion[1]))
  expect_output(print(mar_na), paste(mar_na$meta$proportion[2]))
  expect_output(print(mar_na), paste(mar_na$meta$proportion[3]))
  expect_output(print(mar_na), paste(mar_na$meta$proportion[4]))

  expect_output(print(mar_no_win_odds),
                paste(mar_no_win_odds$meta$n_active[1]))
  expect_output(print(mar_no_win_odds),
                paste(mar_no_win_odds$meta$n_active[2]))
  expect_output(print(mar_no_win_odds),
                paste(mar_no_win_odds$meta$n_active[3]))
  expect_output(print(mar_no_win_odds),
                paste(mar_no_win_odds$meta$n_active[4]))

})

test_that("Maraca plotting", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels,
    column_names,
    fixed_followup_days
    )
  plot(mar)
  expect_true(TRUE)
})

test_that("Test plot functions only work with maraca objects", {
  expect_error(plot_maraca(123), regexp = "Must inherit")
})

test_that("Validation function for  maraca plots", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels,
    column_names,
    fixed_followup_days,
    compute_win_odds = TRUE
  )

  a_def <- plot(mar, density_plot_type = "default")
  a_violin <- plot(mar, density_plot_type = "violin")
  a_box <- plot(mar, density_plot_type = "box")
  a_scatter <- plot(mar, density_plot_type = "scatter")
  val_res_def <- validate_maraca(a_def)
  val_res_violin <- validate_maraca(a_violin)
  val_res_box <- validate_maraca(a_box)
  val_res_scatter <- validate_maraca(a_scatter)

  expect_type(val_res_def, "list")
  expect_type(val_res_violin, "list")
  expect_type(val_res_box, "list")
  expect_type(val_res_scatter, "list")

  expected_names <- c("plot_type", "proportions",
                      "tte_data", "scatter_data",
                      "boxstat_data", "violin_data",
                      "wo_stats")
  expect_named(val_res_def, expected_names, ignore.order = TRUE)
  expect_named(val_res_violin, expected_names, ignore.order = TRUE)
  expect_named(val_res_box, expected_names, ignore.order = TRUE)
  expect_named(val_res_scatter, expected_names, ignore.order = TRUE)

  expect_equal(val_res_def$plot_type, "GeomViolin")
  expect_equal(val_res_violin$plot_type, "GeomViolin")
  expect_equal(val_res_box$plot_type, "GeomBoxplot")
  expect_equal(val_res_scatter$plot_type, "GeomPoint")

  expected_names <- c(tte_outcomes, continuous_outcome)
  expect_named(val_res_def$proportions, expected_names, ignore.order = TRUE)
  expect_named(val_res_violin$proportions, expected_names, ignore.order = TRUE)
  expect_named(val_res_box$proportions, expected_names, ignore.order = TRUE)
  expect_named(val_res_scatter$proportions, expected_names, ignore.order = TRUE)

  expect_equal(unname(val_res_def$proportions), mar$meta$proportion)
  expect_equal(unname(val_res_violin$proportions), mar$meta$proportion)
  expect_equal(unname(val_res_box$proportions), mar$meta$proportion)
  expect_equal(unname(val_res_scatter$proportions), mar$meta$proportion)

  mar_tte_dat <- as.data.frame(mar$ecdf_by_outcome$data)
  mar_tte_dat <- mar_tte_dat[order(mar_tte_dat$adjusted.time), ]
  val_res_def$tte_data <- val_res_def$tte_data[order(val_res_def$tte_data$x), ]
  val_res_violin$tte_data <-
    val_res_violin$tte_data[order(val_res_violin$tte_data$x), ]
  val_res_box$tte_data <- val_res_box$tte_data[order(val_res_box$tte_data$x), ]
  val_res_scatter$tte_data <-
    val_res_scatter$tte_data[order(val_res_scatter$tte_data$x), ]
  expect_equal(val_res_def$tte_data$x, mar_tte_dat$adjusted.time)
  expect_equal(val_res_violin$tte_data$x, mar_tte_dat$adjusted.time)
  expect_equal(val_res_box$tte_data$x, mar_tte_dat$adjusted.time)
  expect_equal(val_res_scatter$tte_data$x, mar_tte_dat$adjusted.time)
  expect_equal(val_res_def$tte_data$y, mar_tte_dat$ecdf_values)
  expect_equal(val_res_violin$tte_data$y, mar_tte_dat$ecdf_values)
  expect_equal(val_res_box$tte_data$y, mar_tte_dat$ecdf_values)
  expect_equal(val_res_scatter$tte_data$y, mar_tte_dat$ecdf_values)
  expect_equal(val_res_def$tte_data$group, mar_tte_dat$arm)
  expect_equal(val_res_violin$tte_data$group, mar_tte_dat$arm)
  expect_equal(val_res_box$tte_data$group, mar_tte_dat$arm)
  expect_equal(val_res_scatter$tte_data$group, mar_tte_dat$arm)

  expect_null(val_res_def$scatter_data)
  expect_null(val_res_violin$boxstat_data)
  expect_null(val_res_violin$scatter_data)
  expect_null(val_res_box$scatter_data)
  expect_null(val_res_box$violin_data)
  expect_null(val_res_scatter$violin_data)
  expect_null(val_res_scatter$boxstat_data)

  expect_equal(sort(val_res_scatter$scatter_data$x),
               sort(mar$continuous$data$x))

  y_values <- unique(mar$continuous$data[, c("arm", "y_level")])
  y_values <- y_values[order(y_values$arm), ]
  jitter_means <- val_res_scatter$scatter_data %>%
                        dplyr::group_by(group) %>%
                        dplyr::summarize("y_level" = mean(y))
  expect_equal(jitter_means$y_level, y_values$y_level, tolerance = 0.1)

  boxplot_stats <- mar$continuous$data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarize("perc_25th" = unname(quantile(x, probs = 0.25)),
                     "median" = median(x),
                     "perc_75th" = unname(quantile(x, probs = 0.75)),
                     "lower_whisker" = min(x[x >= (perc_25th - (perc_75th -
                                                      perc_25th) * 1.5)]),
                     "upper_whisker" = max(x[x <= (perc_75th + (perc_75th -
                                                      perc_25th) * 1.5)]),
                     "x_lowest" = min(x),
                     "x_highest" = max(x))
  expect_equal(val_res_def$boxstat_data$x_lowest, boxplot_stats$x_lowest)
  expect_equal(val_res_def$boxstat_data$whisker_lower,
               boxplot_stats$lower_whisker)
  expect_equal(val_res_def$boxstat_data$hinge_lower, boxplot_stats$perc_25th)
  expect_equal(val_res_def$boxstat_data$median, boxplot_stats$median)
  expect_equal(val_res_def$boxstat_data$hinge_upper, boxplot_stats$perc_75th)
  expect_equal(val_res_def$boxstat_data$whisker_upper,
               boxplot_stats$upper_whisker)
  expect_equal(val_res_def$boxstat_data$x_highest, boxplot_stats$x_highest)
  expect_equal(val_res_box$boxstat_data$x_lowest, boxplot_stats$x_lowest)
  expect_equal(val_res_box$boxstat_data$whisker_lower,
               boxplot_stats$lower_whisker)
  expect_equal(val_res_box$boxstat_data$hinge_lower, boxplot_stats$perc_25th)
  expect_equal(val_res_box$boxstat_data$median, boxplot_stats$median)
  expect_equal(val_res_box$boxstat_data$hinge_upper, boxplot_stats$perc_75th)
  expect_equal(val_res_box$boxstat_data$whisker_upper,
               boxplot_stats$upper_whisker)
  expect_equal(val_res_box$boxstat_data$x_highest, boxplot_stats$x_highest)

  y_values_violin <- unique(val_res_violin$violin_data$y)
  violin_stats <- mar$continuous$data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarize("mean" = mean(x))
  violin_stats_from_plot <- val_res_violin$violin_data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize("mean" = weighted.mean(x, density))
  expect_equal(y_values_violin, y_values$y_level)
  expect_equal(violin_stats_from_plot$mean, violin_stats$mean, tolerance = 0.1)

})

test_that("Test win odds extraction of validation function", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar_with_win_odds <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels,
    column_names,
    fixed_followup_days,
    compute_win_odds = TRUE
  )
  mar_without_win_odds <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels,
    column_names,
    fixed_followup_days,
    compute_win_odds = FALSE
  )
  a_with_wo <- plot(mar_with_win_odds)
  a_without_wo <- plot(mar_without_win_odds)
  val_res_with_wo <- validate_maraca(a_with_wo)
  val_res_without_wo <- validate_maraca(a_without_wo)

  expect_type(val_res_with_wo$wo_stats, "double")
  expect_named(val_res_with_wo$wo_stats,
               c("winodds", "lowerCI", "upperCI", "p_value"),
               ignore.order = TRUE)

  data <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
                                   arm_levels, column_names = column_names
                                   )
  win_odds <- .compute_win_odds(data)
  expect_equivalent(val_res_with_wo$wo_stats["winodds"], win_odds["estimate"])
  expect_equivalent(val_res_with_wo$wo_stats["lowerCI"], win_odds["lower"])
  expect_equivalent(val_res_with_wo$wo_stats["upperCI"], win_odds["upper"])
  expect_equivalent(val_res_with_wo$wo_stats["p_value"], win_odds["p-value"])

  expect_null(val_res_without_wo$wo_stats)
})

test_that("Validation function only works for maraca plot", {
  tmp <- data.frame("a" = 1:10, "b" = 10:1)
  plot <- ggplot2::ggplot(tmp, ggplot2::aes(a, b)) +
    ggplot2::geom_point()

  expect_error(validate_maraca(plot), regexp =
              "Must inherit from class")
})

test_that("Test reformatting of data", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names
  )

  expect_equal(class(data), "data.frame")
  expect_equal(class(data$arm), "factor")
  expect_equal(levels(data$arm), names(arm_levels))
  expect_equal(class(data$outcome), "factor")
  expect_equal(levels(data$outcome), c(tte_outcomes, continuous_outcome))

})

test_that("Test win odds", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
    arm_levels, column_names = column_names
  )
  win_odds <- .compute_win_odds(data)

  expect_equal(class(win_odds), "numeric")
  expect_equal(
    unname(win_odds), c(1.3143433745, 1.1364670136, 1.5200604024, 0.000191286)
  )

})

test_that("Test compute metainfo", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
    arm_levels,
    column_names = column_names)
  metainfo <- .compute_metainfo(data)
  expect_equal(
    as.character(metainfo$outcome), c(tte_outcomes, continuous_outcome))
  expect_equal(metainfo$n, c(129, 115, 110, 77, 569))
  expect_equal(metainfo$proportion, c(12.9, 11.5, 11, 7.7, 56.9))
  expect_equal(
    metainfo$maxday,
    c(1074.68287, 1068.22797, 1074.46617, 1028.40857, 63.76411),
    tol = 1e-5
  )
  expect_equal(metainfo$startx, c(0, 12.9, 24.4, 35.4, 43.1))
  expect_equal(metainfo$endx, c(12.9, 24.4, 35.4, 43.1, 100))
  expect_equal(metainfo$starty, c(0, 0, 0, 0, 0))
  expect_equal(metainfo$n.groups, c(5, 5, 5, 5, 5))
  expect_equal(metainfo$n_active, c(63, 55, 50, 34, 298))
  expect_equal(metainfo$n_control, c(66, 60, 60, 43, 271))
})

test_that("Test compute ecdf", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
    arm_levels,
    column_names
  )
  meta <- .compute_metainfo(data)
  HCE_ecdf <- .compute_ecdf_by_outcome(
    data, meta, tte_outcomes, continuous_outcome, arm_levels, 3 * 365)

  # Checking the abssum along the columns to check that values remain the same.
  expect_equal(sum(abs(HCE_ecdf$data$value)), 221627.7286)
  expect_equal(sum(abs(HCE_ecdf$data$t_cdf)), 841397.7286)
  expect_equal(sum(abs(HCE_ecdf$data$ecdf_values)), 9367.6)
  expect_equal(sum(abs(HCE_ecdf$data$adjusted.time)), 9142.184244)

  expect_equal(
    HCE_ecdf$meta$max,
    c(12.6, 23.6, 33.6, 40.4, 13.2, 25.2, 37.2, 45.8), tol = 1e-6)
  expect_equal(HCE_ecdf$meta$sum.event, c(
    63, 55, 50, 34, 66, 60, 60, 43
  ))
  expect_equal(HCE_ecdf$meta$ecdf_end,
    c(40.4, 40.4, 40.4, 40.4, 45.8, 45.8, 45.8, 45.8), tol = 1e-6
  )

})


test_that("Test compute continuous", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
    arm_levels, column_names = column_names)
  meta <- .compute_metainfo(data)
  HCE_ecdf <- .compute_ecdf_by_outcome(
    data, meta, tte_outcomes, continuous_outcome, arm_levels, 3 * 365)
  continuous <- .compute_continuous(
    data, meta, HCE_ecdf, tte_outcomes, continuous_outcome, arm_levels)
  expect_equal(sum(abs(continuous$data$x)), 40828.387)
  expect_equal(sum(abs(continuous$data$y_level)), 24451)

  expect_equal(continuous$meta$n, c(298, 271))
  expect_equal(continuous$meta$median, c(74.360287, 68.354528))
  expect_equal(continuous$meta$average, c(73.72377, 69.5893123))
})


test_that("Test error for missing outcome", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome XXX"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  expect_error(
    maraca(data, tte_outcomes, continuous_outcome, arm_levels,
           column_names, 3 * 365),
    regexp = "Outcome Outcome XXX is not present in column GROUP"
  )
})

test_that("Test compute win_odds flag", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  expect_true(is.null(mar$win_odds))

  plot(mar)

})

test_that("Test handle NA data", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  data$AVAL0[[3]] <- NA
  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365
  )

  plot(mar)
  expect_true(TRUE)
})

test_that("Test modify continuous x grid", {
  file <- fixture_path("hce_scenario_c.csv")
  args <- .maraca_args(file)
  mar <- maraca(
    args$data,
    args$tte_outcomes,
    args$continuous_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365
  )

  expect_true(TRUE)

  plot(mar, continuous_grid_spacing_x = 8)
})

test_that("Test apply transformation to continuous scale", {
  file <- fixture_path("hce_scenario_c.csv")
  args <- .maraca_args(file)
  mar <- maraca(
    args$data,
    args$tte_outcomes,
    args$continuous_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365
  )

  expect_true(TRUE)

  plot(mar, trans = "sqrt")

})

test_that("Test density plot selection", {
  file <- fixture_path("hce_scenario_c.csv")
  args <- .maraca_args(file)
  mar <- maraca(
    args$data,
    args$tte_outcomes,
    args$continuous_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365
  )

  expect_true(TRUE)

  plot(mar, density_plot_type = "default")
  plot(mar, density_plot_type = "violin")
  plot(mar, density_plot_type = "box")
  suppressWarnings(
    plot(mar, density_plot_type = "scatter")
  )
})

test_that("Test vline type", {
  file <- fixture_path("hce_scenario_c.csv")
  args <- .maraca_args(file)
  mar <- maraca(
    args$data,
    args$tte_outcomes,
    args$continuous_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365
  )

  expect_true(TRUE)

  plot(mar, vline_type = "median")
  plot(mar, vline_type = "mean")
})

test_that("test ordered column", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  hce <- .reformat_and_check_data(data, tte_outcomes, continuous_outcome,
    arm_levels, column_names)

  hce <- .with_ordered_column(hce)

  # Verify against the ones we calculated in the fixture
  expect_equal(data$AVAL, hce$ordered, tol = 1e-7)
})

test_that("test minor_grid", {
  grid <- .minor_grid(c(-10, -2, 3, 27), 1, 5)
  expect_equal(grid, c(-10, -5, 0, 5, 10, 15, 20, 25))

  grid <- .minor_grid(c(-10, -7, -3, -2), 1, 5)
  expect_equal(grid, c(-10, -5))

  grid <- .minor_grid(c(2, 3, 7, 10, 14), 1, 5)
  expect_equal(grid, c(5, 10))
})

test_that("test minor_grid", {
  rates_A <- c(1.72, 1.74, 0.58, 1.5, 1)
  rates_P <- c(2.47, 2.24, 2.9, 4, 6)
  HCE <- hce::simHCE(
    n = 2500, TTE_A = rates_A, TTE_P = rates_P,
    CM_A = -3, CM_P = -6, CSD_A = 16, CSD_P = 15, fixedfy = 3,
    seed = 31337)
  plot(HCE)
  expect_true(TRUE)
})
