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

expect_text_equal <- function(result, expected) {
  expect_equal(length(result), length(expected))

  for (i in seq_along(expected)) {
    expect_equal(result[[i]], expected[[i]])
  }

}

test_that("createMaracaObject", {
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

  # Internal checks
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
  hce_ecdf <- .compute_ecdf_by_outcome(data, meta, tte_outcomes,
                                       continuous_outcome,
                                       arm_levels, 3 * 365)
  continuous <- .compute_continuous(data, meta, hce_ecdf,
                                    tte_outcomes, continuous_outcome,
                                    arm_levels)
  expect_equal(sum(abs(continuous$data$x)), 40828.387)
  expect_equal(sum(abs(continuous$data$y_level)), 24451)

  expect_equal(continuous$meta$n, c(298, 271))
  expect_equal(continuous$meta$median, c(74.360287, 68.354528))
  expect_equal(continuous$meta$average, c(73.72377, 69.5893123))

  # Test reformatting of data
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

  # Test compute metainfo
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
  expect_equal(as.character(metainfo$outcome),
               c(tte_outcomes, continuous_outcome))
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

  # Test Compute ECDF
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
  hce_ecdf <- .compute_ecdf_by_outcome(data, meta, tte_outcomes,
                                       continuous_outcome, arm_levels, 3 * 365)

  # Checking the abssum along the columns to check that values remain the same.
  expect_equal(sum(abs(hce_ecdf$data$value)), 221627.7286)
  expect_equal(sum(abs(hce_ecdf$data$t_cdf)), 841397.7286)
  expect_equal(sum(abs(hce_ecdf$data$ecdf_values)), 9367.6)
  expect_equal(sum(abs(hce_ecdf$data$adjusted.time)), 9142.184244)

  expect_equal(hce_ecdf$meta$max,
               c(12.6, 23.6, 33.6, 40.4, 13.2,
                 25.2, 37.2, 45.8), tol = 1e-6)
  expect_equal(hce_ecdf$meta$sum.event, c(
    63, 55, 50, 34, 66, 60, 60, 43
  ))
  expect_equal(hce_ecdf$meta$ecdf_end,
    c(40.4, 40.4, 40.4, 40.4, 45.8, 45.8, 45.8, 45.8), tol = 1e-6
  )

  # test ordered column
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

  # test minor_grid
  grid <- .minor_grid(c(-10, -2, 3, 27), 1, 5)
  expect_equal(grid, c(-10, -5, 0, 5, 10, 15, 20, 25))

  grid <- .minor_grid(c(-10, -7, -3, -2), 1, 5)
  expect_equal(grid, c(-10, -5))

  grid <- .minor_grid(c(2, 3, 7, 10, 14), 1, 5)
  expect_equal(grid, c(5, 10))
})

test_that("alternativeActiveControl", {
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
})

test_that("alternativeColumnNames", {
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
})

test_that("wrongParameters", {
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
    maraca(data, tte_outcomes, continuous_outcome,
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

  # Test plot functions only work with maraca objects
  expect_error(plot_maraca(123), regexp = "Must inherit")

  # Test plot.hce input
  rates_a <- c(1.72, 1.74, 0.58, 1.5, 1)
  rates_p <- c(2.47, 2.24, 2.9, 4, 6)
  hce_dat <- hce::simHCE(n = 2500, TTE_A = rates_a,
                         TTE_P = rates_p, CM_A = -3,
                         CM_P = -6, CSD_A = 16, CSD_P = 15,
                         fixedfy = 3, seed = 31337)
  hce_dat$TTEfixed <- NULL
  hce_dat$PADY <- NULL
  expect_error(plot(hce_dat))

  # Validation function only works for maraca plot
  tmp <- data.frame("a" = 1:10, "b" = 10:1)
  plot <- ggplot2::ggplot(tmp, ggplot2::aes(a, b)) +
    ggplot2::geom_point()

  expect_error(validate_maraca_plot(plot), regexp =
                 "Must inherit from class")

  # Check missing outcome
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

test_that("winOddsData", {
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

  mar_no_win_odds <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  res <- capture.output(mar)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds (95% CI): 1.31 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  77        7.7       34        43       0",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  res <- capture.output(mar_no_win_odds)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds not calculated.", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  77        7.7       34        43       0",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  res <- capture.output(mar_na)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 999 patients."),
              "", "1 patient(s) removed because of missing values.", "",
              "Win odds (95% CI): 1.32 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  76        7.6       33        43       1",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

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

  # win odds missing if set to false
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

})

test_that("winOddsPlot", {
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

  output <- artifacts_path("winOddsPlot-with.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

  mar <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  output <- artifacts_path("winOddsPlot-without.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

})


test_that("winOddsPrinting", {
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

  mar_no_win_odds <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  res <- capture.output(mar)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds (95% CI): 1.31 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  77        7.7       34        43       0",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  res <- capture.output(mar_no_win_odds)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds not calculated.", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  77        7.7       34        43       0",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  res <- capture.output(mar_na)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 999 patients."),
              "", "1 patient(s) removed because of missing values.", "",
              "Win odds (95% CI): 1.32 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  76        7.6       33        43       1",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

})

test_that("maracaPrinting", {
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

  mar_no_win_odds <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, tte_outcomes, continuous_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  res <- capture.output(mar)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds (95% CI): 1.31 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  77        7.7       34        43       0",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  res <- capture.output(mar_no_win_odds)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds not calculated.", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  77        7.7       34        43       0",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  res <- capture.output(mar_na)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 999 patients."),
              "", "1 patient(s) removed because of missing values.", "",
              "Win odds (95% CI): 1.32 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            OUTCOME   N PROPORTION N_ACTIVE N_CONTROL MISSING",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  76        7.6       33        43       1",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

})

test_that("maracaPlotting", {
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

  output <- artifacts_path("maracaPlotting-basic.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

  output <- artifacts_path("maracaPlotting-none.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, theme = "none")
  expect_file_exists(output)

  output <- artifacts_path("maracaPlotting-maraca.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, theme = "maraca")
  expect_file_exists(output)

  output <- artifacts_path("maracaPlotting-maraca-old.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, theme = "maraca_old")
  expect_file_exists(output)

  output <- artifacts_path("maracaPlotting-color1.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, theme = "color1")
  expect_file_exists(output)

  output <- artifacts_path("maracaPlotting-color2.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, theme = "color2")
  expect_file_exists(output)

  expect_error(plot(mar, theme = "my_theme"),
               regexp = "Please provide theme that exists")
})

test_that("validationFunction", {
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

  output <- artifacts_path("validationFunction-default.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  a_def <- plot(mar, density_plot_type = "default")
  expect_file_exists(output)

  output <- artifacts_path("validationFunction-violin.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  a_violin <- plot(mar, density_plot_type = "violin")
  expect_file_exists(output)

  output <- artifacts_path("validationFunction-box.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  a_box <- plot(mar, density_plot_type = "box")
  expect_file_exists(output)

  output <- artifacts_path("validationFunction-scatter.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  a_scatter <- plot(mar, density_plot_type = "scatter")
  expect_file_exists(output)

  val_res_def <- validate_maraca_plot(a_def)
  val_res_violin <- validate_maraca_plot(a_violin)
  val_res_box <- validate_maraca_plot(a_box)
  val_res_scatter <- validate_maraca_plot(a_scatter)

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
                     "lower_whisker" = min(x[x >= (perc_25th -
                                                     (perc_75th -
                                                        perc_25th) * 1.5)]),
                     "upper_whisker" = max(x[x <= (perc_75th +
                                                     (perc_75th -
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

test_that("handleNAData", {
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

  output <- artifacts_path("handleNAData-basic.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)
})

test_that("gridSpacing", {
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

  output <- artifacts_path("gridSpacing-basic.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, continuous_grid_spacing_x = 8)
  expect_file_exists(output)
})

test_that("scaleTransform", {
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

  output <- artifacts_path("scaleTransform-basic.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, trans = "sqrt")
  expect_file_exists(output)

})

test_that("densityPlotType", {
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

  output <- artifacts_path("densityPlotType-default.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, density_plot_type = "default")
  expect_file_exists(output)

  output <- artifacts_path("densityPlotType-violin.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, density_plot_type = "violin")
  expect_file_exists(output)

  output <- artifacts_path("densityPlotType-box.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, density_plot_type = "box")
  expect_file_exists(output)

  output <- artifacts_path("densityPlotType-scatter.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  suppressWarnings(
    plot(mar, density_plot_type = "scatter")
  )
  expect_file_exists(output)
})

test_that("verticalLine", {
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

  output <- artifacts_path("verticalLine-median.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, vline_type = "median")
  expect_file_exists(output)

  output <- artifacts_path("verticalLine-mean.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, vline_type = "mean")
  expect_file_exists(output)
})

test_that("plotHCE", {
  rates_a <- c(1.72, 1.74, 0.58, 1.5, 1)
  rates_p <- c(2.47, 2.24, 2.9, 4, 6)
  hce_dat <- hce::simHCE(n = 2500, TTE_A = rates_a,
                         TTE_P = rates_p, CM_A = -3,
                         CM_P = -6, CSD_A = 16,
                         CSD_P = 15, fixedfy = 3,
                         seed = 31337)
  output <- artifacts_path("plotHCE-basic.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(hce_dat)
  expect_file_exists(output)

  output <- artifacts_path("plotHCE-fixed_follow_up.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(hce_dat, fixed_followup_days = 6 * 365)
  expect_file_exists(output)

  if (!("PADY" %in% names(hce_dat))) {
    hce_dat$PADY <- hce_dat$TTEfixed
  }
  hce_dat$TTEfixed <- NULL
  output <- artifacts_path("plotHCE-newVersionHCEpkg.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(hce_dat)
  expect_file_exists(output)

})
