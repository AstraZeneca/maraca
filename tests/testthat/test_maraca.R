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

test_that("plot_tte_components", {
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
  print(plot_tte_components(mar))
  expect_true(TRUE)
})

test_that("Initialisation without fixed_followup_days", {
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
  mar <- maraca(data, tte_outcomes, continuous_outcome, arm_levels,
                column_names = column_names)
  expect_s3_class(mar, "maraca")
  expect_equal(mar$fixed_followup_days, 1074.68286)
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
      fixed_followup_days), regexp = "Must have length 2"
  )
  expect_error(
    maraca(data, tte_outcomes, continuous_outcome, arm_levels,
           fixed_followup_days = 12.3
          ),
    regexp = "single integerish value"
  )

  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c("a"),
      12
    ),
    regexp = "Must have length 3"
  )
  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c("a", "b", "c"),
      12
    ),
    regexp = "Must have names"
  )
  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c(foo = "a", bar = "b", baz = "c"),
      12
    ),
    regexp = "Names must be a identical to"
  )

  expect_error(
    maraca(
      data, tte_outcomes, continuous_outcome, arm_levels,
      c(
        outcome = "GROUP", arm = "notexistent",
        value = "AVAL0"
      ), 12
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
      ), 12
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
      ), 12
    ),
    regexp = "The outcome column must be characters"
  )
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


test_that("Test plot_tte_composite", {
  file <- fixture_path("hce_scenario_a.csv")
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
  print(plot_tte_composite(mar))
  expect_true(TRUE)
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

test_that("Test plot functions only work with maraca objects", {
  expect_error(plot_maraca(123), regexp = "Must inherit")
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

test_that("Test compute survmod", {
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
  survmod <- .compute_survmod_by_outcome(
    data, meta, tte_outcomes, continuous_outcome, arm_levels, 3 * 365)

  # Checking the abssum along the columns to check that values remain the same.
  expect_equal(sum(abs(survmod$data$time)), 225699.2035)
  expect_equal(sum(abs(survmod$data$n.risk)), 170235)
  expect_equal(sum(abs(survmod$data$n.event)), 435)
  expect_equal(sum(abs(survmod$data$n.censor)), 0)
  expect_equal(sum(abs(survmod$data$surv)), 339.6)
  expect_equal(sum(abs(survmod$data$std.err)), 10.10464444)
  expect_equal(sum(abs(survmod$data$upper)), 354.3805911)
  expect_equal(sum(abs(survmod$data$lower)), 325.5440402)
  expect_equal(sum(abs(survmod$data$adjusted.time)), 9487.798376)
  expect_equal(sum(abs(survmod$data$km.y)), 95.4)
  expect_equal(sum(abs(survmod$data$max)), 11902.4)
  expect_equal(sum(abs(survmod$data$sum.event)), 24209)
  expect_equal(sum(abs(survmod$data$km.end)), 18821.4)

  expect_equal(
    survmod$meta$max,
    c(12.6, 23.6, 33.6, 40.4, 13.2, 25.2, 37.2, 45.8), tol = 1e-6)
  expect_equal(survmod$meta$sum.event, c(
    63, 55, 50, 34, 66, 60, 60, 43
  ))
  expect_equal(survmod$meta$km.end,
    c(40.4, 40.4, 40.4, 40.4, 45.8, 45.8, 45.8, 45.8), tol = 1e-6
  )

})


test_that("Test compute survmod", {
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
    arm_levels, column_names)
  meta <- .compute_metainfo(data)
  fixed_followup_days <- max(meta[meta$outcome %in% tte_outcomes, ]$maxday)
  survmod <- .compute_survmod_by_outcome(
    data, meta, tte_outcomes, continuous_outcome, arm_levels,
    fixed_followup_days)

  expect_equal(sum(abs(survmod$data$time)), 224624.521)
  expect_equal(sum(abs(survmod$data$km.end)), 18775.6)

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
  survmod <- .compute_survmod_by_outcome(
    data, meta, tte_outcomes, continuous_outcome, arm_levels, 3 * 365)
  continuous <- .compute_continuous(
    data, meta, survmod, tte_outcomes, continuous_outcome, arm_levels)
  expect_equal(sum(abs(continuous$data$x)), 40828.387)
  expect_equal(sum(abs(continuous$data$violiny)), 24451)

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
    maraca(data, tte_outcomes, continuous_outcome, arm_levels, column_names),
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
    data, tte_outcomes, continuous_outcome, arm_levels, column_names,
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
    data, tte_outcomes, continuous_outcome, arm_levels, column_names
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
    args$column_names
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
    args$column_names
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
    args$column_names
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
    args$column_names
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

test_that("test hce focus function", {
  file <- fixture_path("hce_scenario_a.csv")
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
  fixed_followup_days <- 3 * 365

  .hce_survival_focus(data, 4, tte_outcomes, fixed_followup_days)
  expect_true(TRUE)
})

test_that("test minor_grid", {
  grid <- .minor_grid(c(-10, -2, 3, 27), 1, 5)
  expect_equal(grid, c(-10, -5, 0, 5, 10, 15, 20, 25))

  grid <- .minor_grid(c(-10, -7, -3, -2), 1, 5)
  expect_equal(grid, c(-10, -5))

  grid <- .minor_grid(c(2, 3, 7, 10, 14), 1, 5)
  expect_equal(grid, c(5, 10))
})
