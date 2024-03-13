.maraca_args <- function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  return(list(
    data = data,
    step_outcomes = step_outcomes,
    last_outcome = last_outcome,
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

  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  fixed_followup_days <- 3 * 365
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels,
    column_names,
    fixed_followup_days
  )
  expect_s3_class(mar, "maraca")
  expect_equal(mar$fixed_followup_days, fixed_followup_days)

  # Internal checks
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, step_outcomes, last_outcome,
                                   arm_levels, column_names = column_names)
  meta <- .compute_metainfo(data)

  step_types <- rep("tte", times = length(step_outcomes))
  hce_ecdf <- .compute_ecdf_by_outcome(data, meta, step_outcomes,
                                       step_types = step_types,
                                       last_outcome,
                                       arm_levels, 3 * 365)
  continuous <- .compute_continuous(data, meta, hce_ecdf,
                                    step_outcomes, last_outcome,
                                    arm_levels)
  expect_equal(sum(abs(continuous$data$x)), 40828.387)
  expect_equal(sum(abs(continuous$data$y)), 24451)

  expect_equal(continuous$meta$n, c(298, 271))
  expect_equal(continuous$meta$median, c(74.360287, 68.354528))
  expect_equal(continuous$meta$average, c(73.72377, 69.5893123))

  # Test reformatting of data
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(
    data, step_outcomes, last_outcome, arm_levels, column_names
  )

  expect_equal(class(data), "data.frame")
  expect_equal(class(data$arm), "factor")
  expect_equal(levels(data$arm), unname(arm_levels))
  expect_equal(class(data$outcome), "factor")
  expect_equal(levels(data$outcome), c(step_outcomes, last_outcome))

  # Test compute metainfo
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, step_outcomes, last_outcome,
                                   arm_levels,
                                   column_names = column_names)
  metainfo <- .compute_metainfo(data)
  expect_equal(as.character(metainfo$outcome),
               c(step_outcomes, last_outcome))
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
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, step_outcomes, last_outcome,
    arm_levels,
    column_names
  )
  meta <- .compute_metainfo(data)

  step_types <- rep("tte", times = length(step_outcomes))
  hce_ecdf <- .compute_ecdf_by_outcome(data, meta, step_outcomes,
                                       step_types = step_types,
                                       last_outcome, arm_levels, 3 * 365)

  # Checking the abssum along the columns to check that values remain the same.
  expect_equal(sum(abs(hce_ecdf$data$value)), 221627.7286)
  expect_equal(sum(abs(hce_ecdf$data$t_cdf)), 841397.7286)
  expect_equal(sum(abs(hce_ecdf$data$step_values)), 9367.6)
  expect_equal(sum(abs(hce_ecdf$data$adjusted.time)), 9142.184244)

  expect_equal(hce_ecdf$meta$max,
               c(12.6, 13.2, 23.6, 25.2,
                 33.6, 37.2, 40.4, 45.8), tol = 1e-6)
  expect_equal(hce_ecdf$meta$sum.event, c(
    63, 66, 55, 60, 50, 60, 34, 43
  ))
  expect_equal(hce_ecdf$meta$ecdf_end,
    c(40.4, 45.8, 40.4, 45.8, 40.4, 45.8, 40.4, 45.8), tol = 1e-6
  )

  # test ordered column
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  hce <- .reformat_and_check_data(data, step_outcomes, last_outcome,
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

  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  fixed_followup_days <- 3 * 365
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels,
    column_names,
    fixed_followup_days
  )
  expect_s3_class(mar, "maraca")
})

test_that("alternativeColumnNames", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)

  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  fixed_followup_days <- 3 * 365
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels,
    column_names,
    fixed_followup_days
  )
  expect_s3_class(mar, "maraca")
})

test_that("vectorFixedFollowUp", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)

  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  fixed_followup_days <- ceiling(unname(sapply(step_outcomes, function(tte) {
    max(data[data$GROUP == tte, "AVAL0"])
  })))

  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels,
    column_names,
    fixed_followup_days
  )
  expect_s3_class(mar, "maraca")
})

test_that("wrongParameters", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  expect_error(
    maraca(
      "hello", step_outcomes, last_outcome, arm_levels,
      fixed_followup_days = fixed_followup_days
    ), regexp = "Must be of type 'data\\.frame'"
  )
  expect_error(
    maraca(
      data, c(1, 2, 3), last_outcome, arm_levels,
      fixed_followup_days = fixed_followup_days
    ), regexp = "Must be of type 'character'"

  )
  expect_error(
    maraca(data, step_outcomes, 3, arm_levels,
      fixed_followup_days = fixed_followup_days
    ),
    regexp = "Must be of type 'string'"
  )
  expect_error(
    maraca(data, step_outcomes, last_outcome, c(1, 2),
           fixed_followup_days = fixed_followup_days),
    regexp = "Must be of type 'character'"
  )
  expect_error(
    maraca(data, step_outcomes, last_outcome,
           c(active = "foo", control = "bar", whatever = "baz"),
           column_names,
           fixed_followup_days), regexp = "Must have length 2"
  )
  expect_error(
    maraca(data, step_outcomes, last_outcome, arm_levels,
      column_names,
      fixed_followup_days = "12.3"
    ),
    regexp = "Must be of type 'numeric', not 'character'."
  )
  expect_error(
    maraca(data, step_outcomes, last_outcome, arm_levels,
      column_names,
      fixed_followup_days = NULL
    )
  )
  expect_error(
    maraca(data, step_outcomes, last_outcome, arm_levels,
      column_names,
      fixed_followup_days = 12
    ),
    regexp = "Time-to-event data contain events after the fixed_followup_days"
  )

  expect_error(
    maraca(
      data, step_outcomes, last_outcome, arm_levels,
      c("a"),
      fixed_followup_days
    ),
    regexp = "Must have length 3"
  )
  expect_error(
    maraca(
      data, step_outcomes, last_outcome, arm_levels,
      c("a", "b", "c"),
      fixed_followup_days
    ),
    regexp = "Must have names"
  )
  expect_error(
    maraca(
      data, step_outcomes, last_outcome, arm_levels,
      c(foo = "a", bar = "b", baz = "c"),
      fixed_followup_days
    )
  )

  expect_error(
    maraca(
      data, step_outcomes, last_outcome, arm_levels,
      c(
        outcome = "GROUP", arm = "notexistent",
        value = "AVAL0"
      ), fixed_followup_days
    ),
    regexp = "Can't rename columns that don't exist"
  )

  expect_error(
    maraca(
      data, step_outcomes, last_outcome,
      arm_levels = c(active = "A", control = "C"),
      c(
        outcome = "GROUP", arm = "TRTP",
        value = "AVAL0"
      ), fixed_followup_days
    ),
    regexp = list(paste("Arm variable contains different",
                        "values then given in parameter arm_levels"))
  )

  expect_error(
    maraca(
      data, step_outcomes, last_outcome = "C", arm_levels,
      c(
        outcome = "GROUP", arm = "TRTP",
        value = "AVAL0"
      ), fixed_followup_days
    ),
    regexp = list(paste("Outcome variable contains different",
                        "values then given in parameters",
                        "step_outcomes and last_outcome"))
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
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome XXX"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  expect_error(
    maraca(data, step_outcomes, last_outcome, arm_levels,
           column_names, 3 * 365),
    regexp = list(paste("Outcome variable contains different",
                        "values then given in parameters",
                        "step_outcomes and last_outcome"))
  )
})

test_that("winOddsData", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  mar_no_win_odds <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  res <- capture.output(mar)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds (95% CI): 1.31 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            Outcome   N Proportion N Active N Control Missing",
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
              "            Outcome   N Proportion N Active N Control Missing",
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
              "            Outcome   N Proportion N Active N Control Missing",
              "          Outcome I 129       12.9       63        66       0",
              "         Outcome II 115       11.5       55        60       0",
              "        Outcome III 110       11.0       50        60       0",
              "         Outcome IV  76        7.6       33        43       1",
              " Continuous outcome 569       56.9      298       271       0")

  expect_text_equal(res, exp)

  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  data <- .reformat_and_check_data(data, step_outcomes, last_outcome,
    arm_levels, column_names = column_names
  )

  win_odds_list <- .compute_win_odds(data, arm_levels,
                                     step_outcomes, last_outcome,
                                     lowerBetter = FALSE)
  win_odds <- win_odds_list[["win_odds"]]

  expect_equal(class(win_odds), "numeric")
  expect_equal(
    unname(win_odds), c(1.3143433745, 1.1364670136, 1.5200604024, 0.000191286)
  )

  win_odds_by_outcome <- win_odds_list[["win_odds_outcome"]]
  expect_equal(class(win_odds_by_outcome), "list")
  expect_equal(class(win_odds_by_outcome[["summary"]]), "data.frame")
  expect_equal(class(win_odds_by_outcome[["summary_by_GROUP"]]), "data.frame")
  expect_equal(class(win_odds_by_outcome[["WO"]]), "data.frame")
  expect_equal(names(win_odds_by_outcome[["summary"]]),
               c("TRTP", "WIN", "LOSS", "TIE", "TOTAL", "WR", "WO"))
  expect_equal(names(win_odds_by_outcome[["summary_by_GROUP"]]),
               c("TRTP", "GROUP", "WIN", "LOSS", "TIE", "TOTAL"))
  expect_equal(names(win_odds_by_outcome[["WO"]]),
               c("WO", "SE", "WP", "SE_WP"))

  wo_smry <- win_odds_by_outcome[["summary"]]
  wo_smry_grp <- win_odds_by_outcome[["summary_by_GROUP"]]

  expect_equal(wo_smry[wo_smry$TRTP == "A", "WIN"],
               sum(wo_smry_grp[wo_smry_grp$TRTP == "A", "WIN"]))
  expect_equal(wo_smry[wo_smry$TRTP == "P", "WIN"],
               sum(wo_smry_grp[wo_smry_grp$TRTP == "P", "WIN"]))
  expect_equal(wo_smry[wo_smry$TRTP == "P", "LOSS"],
               sum(wo_smry_grp[wo_smry_grp$TRTP == "P", "LOSS"]))
  expect_equal(wo_smry[wo_smry$TRTP == "A", "TIE"],
               sum(wo_smry_grp[wo_smry_grp$TRTP == "A", "TIE"]))

  wins_II_A <-
    sum(sapply(data[data$arm == "Active" & data$outcome == "Outcome II",
                    "value"],
               function(vl) {
                 sum(vl > data[data$arm == "Control" &
                                 data$outcome == "Outcome II", "value"]) +
                   nrow(data[data$arm == "Control" &
                               data$outcome == "Outcome I", ])
               }))
  expect_equal(wo_smry_grp[wo_smry_grp$TRTP == "A" &
                             wo_smry_grp$GROUP == "Outcome II", "WIN"],
               wins_II_A)

  loss_III_A <-
    sum(sapply(data[data$arm == "Active" & data$outcome == "Outcome III",
                    "value"],
               function(vl) {
                 sum(vl < data[data$arm == "Control" &
                                 data$outcome == "Outcome III", "value"]) +
                   nrow(data[data$arm == "Control" &
                               !(data$outcome %in% c("Outcome I",
                                                     "Outcome II",
                                                     "Outcome III")), ])
               }))

  expect_equal(wo_smry_grp[wo_smry_grp$TRTP == "A" &
                             wo_smry_grp$GROUP == "Outcome III", "LOSS"],
               loss_III_A)

  # win odds missing if set to false
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  expect_true(is.null(mar$win_odds))
  expect_true(is.null(mar$win_odds_outcome))

})

test_that("binaryEndpoints", {

  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Create binary data for last outcome
  idx_cont <- data$GROUP == "Continuous outcome"
  data[idx_cont, "GROUP"] <- "Binary outcome"
  data[idx_cont, "AVAL0"] <- data[idx_cont, "AVAL0"] >= 0
  data[idx_cont, "AVAL"] <- data[idx_cont, "AVAL0"] +
    data[idx_cont, "GROUPN"]

  column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
  )
  step_outcomes <- c("Outcome I", "Outcome II",
                     "Outcome III", "Outcome IV")
  last_outcome <- "Binary outcome"
  arm_levels <- c(active = "Active",
                  control = "Control")

  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names,
    fixed_followup_days = 3 * 365, compute_win_odds = TRUE,
    last_type = "binary"
  )
  dt_last <- mar$data_last_outcome$meta

  binary_data <- data[idx_cont, ]
  idx_active <- binary_data$TRTP == "Active"
  idx_control <- binary_data$TRTP == "Control"
  prop_active <- prop.test(sum(binary_data[idx_active, "AVAL0"] == 1),
                           nrow(binary_data[idx_active, ]))
  prop_control <- prop.test(sum(binary_data[idx_control, "AVAL0"] == 1),
                            nrow(binary_data[idx_control, ]))

  expect_equal(as.numeric(dt_last[dt_last$arm == "Active", "estimate"]),
               100 * unname(prop_active$estimate))
  expect_equal(as.numeric(dt_last[dt_last$arm == "Control", "estimate"]),
               100 * unname(prop_control$estimate))

  lowest_value <- dt_last$estimate - dt_last$ci_diff
  highest_value <- dt_last$estimate + dt_last$ci_diff
  range <- c(min(0, floor(lowest_value / 10) * 10),
             max(100, ceiling(highest_value / 10) * 10))

  expect_equal(as.numeric(dt_last[dt_last$arm == "Active", "average"]),
               unname(.to_rangeab(100 * prop_active$estimate,
                                  max(mar$meta$startx), range[1], range[2])))
  expect_equal(as.numeric(dt_last[dt_last$arm == "Control", "average"]),
               unname(.to_rangeab(100 * prop_control$estimate,
                                  max(mar$meta$startx), range[1], range[2])))

  expect_equal(as.numeric(dt_last[dt_last$arm == "Active", "ci_diff"]),
               100 * unname(prop_active$estimate - prop_active$conf.int[1]))
  expect_equal(as.numeric(dt_last[dt_last$arm == "Control", "ci_diff"]),
               100 * unname(prop_control$estimate - prop_control$conf.int[1]))

  output <- artifacts_path("binary_plot-last.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

  p <- plot(mar)
  val <- validate_maraca_plot(p)

  expect_equal(as.numeric(dt_last[dt_last$arm == "Active", "average"]),
               val$binary_last_data[val$binary_last_data$group == "Active",
                                    "x"])
  expect_equal(as.numeric(dt_last[dt_last$arm == "Control", "average"]),
               val$binary_last_data[val$binary_last_data$group == "Control",
                                    "x"])

  expect_equal(.to_rangeab((as.numeric(dt_last[dt_last$arm == "Active",
                                               "estimate"]) -
                              as.numeric(dt_last[dt_last$arm == "Active",
                                                 "ci_diff"])),
                           max(mar$meta$startx), range[1], range[2]),
               val$binary_last_data[val$binary_last_data$group == "Active",
                                    "lower_ci"])

  expect_equal(.to_rangeab((as.numeric(dt_last[dt_last$arm == "Control",
                                               "estimate"]) +
                              as.numeric(dt_last[dt_last$arm == "Control",
                                                 "ci_diff"])),
                           max(mar$meta$startx), range[1], range[2]),
               val$binary_last_data[val$binary_last_data$group == "Control",
                                    "upper_ci"])

  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Create binary data for step outcome
  idx_bin <- data$GROUP %in% c("Outcome III", "Outcome IV")
  data[idx_bin, "AVAL0"] <- data[idx_bin, "AVAL0"] >= 500
  data[idx_bin, "AVAL"] <- data[idx_bin, "AVAL0"] + data[idx_bin, "GROUPN"]
  data <- data[data$AVAL0 != 0, ]

  column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
  )
  step_outcomes <- c("Outcome I", "Outcome II",
                     "Outcome III", "Outcome IV")
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active",
                  control = "Control")
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names,
    fixed_followup_days = 3 * 365, compute_win_odds = TRUE,
    step_types = c("tte", "tte", "binary", "binary")
  )

  act <- data[data$TRTP == "Active", ]
  ctrl <- data[data$TRTP == "Control", ]

  step_dt <- mar$ecdf_by_outcome$data
  step_act <- step_dt[step_dt$arm == "Active", ]
  step_ctrl <- step_dt[step_dt$arm == "Control", ]

  expect_equal((step_act[step_act$outcome == "Outcome III", "step_values"] -
                  max(step_act[step_act$outcome == "Outcome II",
                               "step_values"])),
               100 * nrow(act[act$GROUP == "Outcome III", ]) / nrow(act))
  expect_equal((step_ctrl[step_ctrl$outcome == "Outcome III", "step_values"] -
                  max(step_ctrl[step_ctrl$outcome == "Outcome II",
                                "step_values"])),
               100 * nrow(ctrl[ctrl$GROUP == "Outcome III", ]) / nrow(ctrl))

  expect_equal((step_act[step_act$outcome == "Outcome IV", "step_values"] -
                  step_act[step_act$outcome == "Outcome III", "step_values"]),
               100 * nrow(act[act$GROUP == "Outcome IV", ]) / nrow(act))
  expect_equal((step_ctrl[step_ctrl$outcome == "Outcome IV", "step_values"] -
                  step_ctrl[step_ctrl$outcome == "Outcome III",
                            "step_values"]),
               100 * nrow(ctrl[ctrl$GROUP == "Outcome IV", ]) / nrow(ctrl))

  output <- artifacts_path("binary_plot-steps.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

  p <- plot(mar)
  val <- validate_maraca_plot(p)
  val_step <- val$binary_step_data

  expect_equal((step_act[step_act$outcome == "Outcome III", "step_values"] -
                  max(step_act[step_act$outcome == "Outcome II",
                               "step_values"])),
               val_step[val_step$group == "Active", "proportion"][1])
  expect_equal((step_ctrl[step_ctrl$outcome == "Outcome III", "step_values"] -
                  max(step_ctrl[step_ctrl$outcome == "Outcome II",
                                "step_values"])),
               val_step[val_step$group == "Control", "proportion"][1])

  expect_equal((step_act[step_act$outcome == "Outcome IV", "step_values"] -
                  step_act[step_act$outcome == "Outcome III", "step_values"]),
               val_step[val_step$group == "Active", "proportion"][2])
  expect_equal((step_ctrl[step_ctrl$outcome == "Outcome IV", "step_values"] -
                  step_ctrl[step_ctrl$outcome == "Outcome III",
                            "step_values"]),
               val_step[val_step$group == "Control", "proportion"][2])

})

test_that("winOddsPlot", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  output <- artifacts_path("winOddsPlot-with.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

  win_odds_outcome <- mar$win_odds_outcome
  wo_smry_grp <- win_odds_outcome$summary_by_GROUP
  endpoints <- c(mar$step_outcomes, mar$last_outcome)
  wo_bar_nc <- .prep_data_component_plot(win_odds_outcome, endpoints,
                                         mar$arm_levels)

  expect_equal(wo_smry_grp[wo_smry_grp$TRTP == "A", "WIN"],
               unname(unlist(wo_bar_nc[wo_bar_nc$count == "Active wins" &
                                         wo_bar_nc$GROUP %in%
                                           c(step_outcomes, last_outcome),
                                       "value"])))
  expect_equal(wo_smry_grp[wo_smry_grp$TRTP == "P", "WIN"],
               unname(unlist(wo_bar_nc[wo_bar_nc$count == "Control wins" &
                                         wo_bar_nc$GROUP %in%
                                           c(step_outcomes, last_outcome),
                                       "value"])))
  expect_equal(win_odds_outcome$summary[win_odds_outcome$summary$TRTP == "A",
                                        "TOTAL"],
               unname(unlist(wo_bar_nc[wo_bar_nc$count == "Active wins",
                                       "total"][1, ])))

  output <- artifacts_path("componentPlot-with.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  component_plot(mar)
  expect_file_exists(output)

  output <- artifacts_path("cumulative_plot-with.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  cumulative_plot(mar)
  expect_file_exists(output)

  output <- artifacts_path("cumulative_plot-reverse.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  cumulative_plot(mar, reverse = TRUE)
  expect_file_exists(output)

  bar_p <- mar$wo_bar
  forest_p <- mar$wins_forest
  sry_by_grp <- win_odds_outcome$summary_by_GROUP
  summary <- win_odds_outcome$summary

  expect_equal(as.numeric(bar_p[bar_p$GROUP == "Outcome I" &
                                  bar_p$count == "Active wins", "value"]),
               sry_by_grp[sry_by_grp$GROUP == "Outcome I" &
                            sry_by_grp$TRTP == "P", "LOSS"])

  expect_equal(as.numeric(bar_p[bar_p$GROUP == "Outcome I" &
                                  bar_p$count == "Control wins", "value"]),
               sry_by_grp[sry_by_grp$GROUP == "Outcome I" &
                            sry_by_grp$TRTP == "A", "LOSS"])

  expect_equal(as.numeric(bar_p[bar_p$GROUP == "Overall" &
                                  bar_p$count == "Active wins", "value"]),
               summary[summary$TRTP == "P", "LOSS"])

  expect_equal(as.numeric(bar_p[bar_p$GROUP == "Overall" &
                                  bar_p$count == "Control wins", "value"]),
               summary[summary$TRTP == "A", "LOSS"])

  expect_equal(forest_p[forest_p$GROUP == "Overall" &
                          forest_p$method == "win odds", "value"],
               win_odds_outcome$WO$WO)

  expect_equal(forest_p[forest_p$GROUP == "Overall" &
                          forest_p$method == "win odds", "LCL"],
               exp(log(win_odds_outcome$WO$WO) -
                     qnorm(0.975) * win_odds_outcome$WO$SE))

  output <- artifacts_path("winOddsPlot-without.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar)
  expect_file_exists(output)

  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  expect_error(component_plot(mar), regexp =
                 list(paste0("Win odds not calculated for maraca object.\n",
                             "  Make sure to set compute_win_odds = TRUE when ",
                             "creating the maraca object.")))

  expect_text_equal(component_plot(data),
                    list(paste0("component_plot() function can only handle ",
                                "inputs of class 'hce' or 'maraca'. ",
                                "Your input has class data.frame.")))

  expect_error(cumulative_plot(mar), regexp =
                 list(paste0("Win odds not calculated for maraca object.\n",
                             "  Make sure to set compute_win_odds = TRUE when ",
                             "creating the maraca object.")))

  expect_text_equal(cumulative_plot(data),
                    list(paste0("cumulative_plot() function can only handle ",
                                "inputs of class 'hce' or 'maraca'. ",
                                "Your input has class data.frame.")))

  rates_a <- c(1.72, 1.74, 0.58, 1.5, 1)
  rates_p <- c(2.47, 2.24, 2.9, 4, 6)
  hce_dat <- hce::simHCE(n = 2500, TTE_A = rates_a,
                         TTE_P = rates_p, CM_A = -3,
                         CM_P = -6, CSD_A = 16,
                         CSD_P = 15, fixedfy = 3,
                         seed = 31337)

  output <- artifacts_path("componentPlot-hce.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  component_plot(hce_dat)
  expect_file_exists(output)

  output <- artifacts_path("cumulative_plot-hce.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  cumulative_plot(hce_dat)
  expect_file_exists(output)

})


test_that("winOddsPrinting", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  mar_no_win_odds <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  res <- capture.output(mar)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds (95% CI): 1.31 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            Outcome   N Proportion N Active N Control Missing",
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
              "            Outcome   N Proportion N Active N Control Missing",
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
              "            Outcome   N Proportion N Active N Control Missing",
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
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  mar_no_win_odds <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = FALSE
  )

  data$AVAL0[[3]] <- NA
  mar_na <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365,
    compute_win_odds = TRUE
  )

  res <- capture.output(mar)
  exp <- list(paste0("Maraca object for plotting maraca ",
                     "graph created for 1000 patients."),
              "", "Win odds (95% CI): 1.31 (1.14, 1.52)",
              "Win odds p-value: <0.001", "",
              "            Outcome   N Proportion N Active N Control Missing",
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
              "            Outcome   N Proportion N Active N Control Missing",
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
              "            Outcome   N Proportion N Active N Control Missing",
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
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels,
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
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  fixed_followup_days <- 3 * 365
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels,
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
                      "tte_data", "binary_step_data",
                      "binary_last_data", "scatter_data",
                      "boxstat_data", "violin_data",
                      "wo_stats")
  expect_named(val_res_def, expected_names, ignore.order = TRUE)
  expect_named(val_res_violin, expected_names, ignore.order = TRUE)
  expect_named(val_res_box, expected_names, ignore.order = TRUE)
  expect_named(val_res_scatter, expected_names, ignore.order = TRUE)

  expect_equal(val_res_def$plot_type, "GeomViolin+GeomBoxplot")
  expect_equal(val_res_violin$plot_type, "GeomViolin")
  expect_equal(val_res_box$plot_type, "GeomBoxplot")
  expect_equal(val_res_scatter$plot_type, "GeomPoint")

  expected_names <- c(step_outcomes, last_outcome)
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
  expect_equal(val_res_def$tte_data$y, mar_tte_dat$step_values)
  expect_equal(val_res_violin$tte_data$y, mar_tte_dat$step_values)
  expect_equal(val_res_box$tte_data$y, mar_tte_dat$step_values)
  expect_equal(val_res_scatter$tte_data$y, mar_tte_dat$step_values)
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
               sort(mar$data_last_outcome$data$x))

  y_values <- unique(mar$data_last_outcome$data[, c("arm", "y")])
  y_values <- y_values[order(y_values$arm), ]
  jitter_means <- val_res_scatter$scatter_data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize("y_level" = mean(y))
  expect_equal(jitter_means$y_level, y_values$y, tolerance = 0.1)

  boxplot_stats <- mar$data_last_outcome$data %>%
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
  violin_stats <- mar$data_last_outcome$data %>%
    dplyr::group_by(arm) %>%
    dplyr::summarize("mean" = mean(x))
  violin_stats_from_plot <- val_res_violin$violin_data %>%
    dplyr::group_by(group) %>%
    dplyr::summarize("mean" = weighted.mean(x, density))
  expect_equal(y_values_violin, y_values$y)
  expect_equal(violin_stats_from_plot$mean, violin_stats$mean, tolerance = 0.1)

})

test_that("handleNAData", {
  file <- fixture_path("hce_scenario_c.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)
  step_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  last_outcome <- "Continuous outcome"
  arm_levels <- c(active = "Active", control = "Control")
  column_names <- c(
    outcome = "GROUP", arm = "TRTP", value = "AVAL0"
  )

  data$AVAL0[[3]] <- NA
  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names, 3 * 365
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
    args$step_outcomes,
    args$last_outcome,
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
  dat <- args$data

  mar <- maraca(
    dat,
    args$step_outcomes,
    args$last_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365
  )

  expect_warning(plot(mar, trans = "log10"),
                 paste("Continuous endpoint has negative values - the log10",
                       "transformation will result in missing values."))
  expect_warning(plot(mar, trans = "log"),
                 paste("Continuous endpoint has negative values - the log",
                       "transformation will result in missing values."))
  expect_warning(plot(mar, trans = "sqrt"),
                 paste("Continuous endpoint has negative values - the sqrt",
                       "transformation will result in missing values."))

  dat[dat$GROUP == "Continuous outcome", "AVAL0"] <-
    dat[dat$GROUP == "Continuous outcome", "AVAL0"] + 50
  mar <- maraca(
    dat,
    args$step_outcomes,
    args$last_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365,
    compute_win_odds = TRUE
  )

  orig_dat <- mar$data_last_outcome$data

  p_log10 <- plot(mar, trans = "log10", density_plot_type = "scatter")
  p_log <- plot(mar, trans = "log", density_plot_type = "scatter")
  p_sqrt <- plot(mar, trans = "sqrt", density_plot_type = "scatter")
  val_log10 <- validate_maraca_plot(p_log10)
  val_log <- validate_maraca_plot(p_log)
  val_sqrt <- validate_maraca_plot(p_sqrt)
  log10_data <- .to_rangeab(log10(orig_dat$value), max(mar$meta$startx),
                            min(log10(orig_dat$value)),
                            max(log10(orig_dat$value)))

  expect_equal(log10_data,
               val_log10$scatter_data$x)
  expect_equal(.to_rangeab(log(orig_dat$value), max(mar$meta$startx),
                           min(log(orig_dat$value)),
                           max(log(orig_dat$value))),
               val_log$scatter_data$x)
  expect_equal(.to_rangeab(sqrt(orig_dat$value), max(mar$meta$startx),
                           min(sqrt(orig_dat$value)),
                           max(sqrt(orig_dat$value))),
               val_sqrt$scatter_data$x)

  p_log10 <- plot(mar, trans = "log10")
  val_log10 <- validate_maraca_plot(p_log10)
  box_dat <- val_log10$boxstat_data

  idx_act <- orig_dat$arm == "Active"
  idx_ctrl <- orig_dat$arm == "Control"

  expect_equal(median(log10_data[idx_act]),
               box_dat[box_dat$group == "Active", "median"])
  expect_equal(unname(quantile(log10_data[idx_act], probs = 0.25)),
               box_dat[box_dat$group == "Active", "hinge_lower"])
  expect_equal(unname(quantile(log10_data[idx_act], probs = 0.75)),
               box_dat[box_dat$group == "Active", "hinge_upper"])
  expect_equal(median(log10_data[idx_ctrl]),
               box_dat[box_dat$group == "Control", "median"])
  expect_equal(unname(quantile(log10_data[idx_ctrl], probs = 0.25)),
               box_dat[box_dat$group == "Control", "hinge_lower"])
  expect_equal(unname(quantile(log10_data[idx_ctrl], probs = 0.75)),
               box_dat[box_dat$group == "Control", "hinge_upper"])

  output <- artifacts_path("scaleTransform-basic.pdf")
  expect_file_not_exists(output)
  set_pdf_output(output)
  plot(mar, trans = "sqrt")
  expect_file_exists(output)

  expect_message(plot(mar, trans = "reverse"),
                 regexp = paste("Last endpoint axis has been reversed, which",
                                "might indicate that lower values are",
                                "considered advantageous. Note that the win",
                                "odds were calculated assuming that higher",
                                "values are better. If that is not correct,",
                                "please use the parameter lowerBetter = TRUE",
                                "in the maraca function."))
  mar <- maraca(
    dat,
    args$step_outcomes,
    args$last_outcome,
    args$arm_levels,
    args$column_names,
    3 * 365,
    lowerBetter = TRUE
  )

  p_reverse <- plot(mar, trans = "reverse", density_plot_type = "scatter")
  val_reverse <- validate_maraca_plot(p_reverse)

  expect_equal(max(mar$meta$startx) - mar$data_last_outcome$data$x + 100,
               val_reverse$scatter_data$x)

  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file, stringsAsFactors = FALSE)

  # Create binary data for last outcome
  idx_cont <- data$GROUP == "Continuous outcome"
  data[idx_cont, "GROUP"] <- "Binary outcome"
  data[idx_cont, "AVAL0"] <- data[idx_cont, "AVAL0"] >= 0
  data[idx_cont, "AVAL"] <- data[idx_cont, "AVAL0"] +
    data[idx_cont, "GROUPN"]

  column_names <- c(
    outcome = "GROUP",
    arm = "TRTP",
    value = "AVAL0"
  )
  step_outcomes <- c("Outcome I", "Outcome II",
                     "Outcome III", "Outcome IV")
  last_outcome <- "Binary outcome"
  arm_levels <- c(active = "Active",
                  control = "Control")

  mar <- maraca(
    data, step_outcomes, last_outcome, arm_levels, column_names,
    fixed_followup_days = 3 * 365, compute_win_odds = TRUE,
    last_type = "binary"
  )

  expect_error(plot(mar, trans = "log10"),
               paste("log10 transformation only implemented for continuous",
                     "last endpoint."))
  expect_error(plot(mar, trans = "log"),
               paste("log transformation only implemented for continuous",
                     "last endpoint."))
  expect_error(plot(mar, trans = "sqrt"),
               paste("sqrt transformation only implemented for continuous",
                     "last endpoint."))

})

test_that("densityPlotType", {
  file <- fixture_path("hce_scenario_c.csv")
  args <- .maraca_args(file)
  mar <- maraca(
    args$data,
    args$step_outcomes,
    args$last_outcome,
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
    args$step_outcomes,
    args$last_outcome,
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
