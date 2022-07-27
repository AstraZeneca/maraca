test_that("Maraca initialisation", {
  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  treatments <- c("Active", "Control")
  fixed_followup_days <- 3 * 365
  mar <- maraca(
    data, tte_outcomes, continuous_outcome, treatments, fixed_followup_days)
  expect_true(TRUE)
  expect_s3_class(mar, "maraca::maraca")
  plot(mar)
  print(plot_tte_trellis(mar))
})

test_that("Test reformatting of data", {
  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  treatments <- c("Active", "Control")
  data <- .reformat_data(data, tte_outcomes, continuous_outcome, treatments)

  expect_equal(class(data), "data.frame")
  expect_equal(class(data$TRTP), "factor")
  expect_equal(levels(data$TRTP), treatments)
  expect_equal(class(data$GROUP), "factor")
  expect_equal(levels(data$GROUP), c(tte_outcomes, continuous_outcome))

})

test_that("Test win odds", {
  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  treatments <- c("Active", "Control")
  data <- .reformat_data(data, tte_outcomes, continuous_outcome, treatments)
  win_odds <- .compute_win_odds(data)

  expect_equal(class(win_odds), "numeric")
  expect_equal(
    unname(win_odds), c(1.3143433745, 1.1377185801, 1.5227833918, 0.0001927074)
  )

})

test_that("Test compute metainfo", {
  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  treatments <- c("Active", "Control")
  data <- .reformat_data(data, tte_outcomes, continuous_outcome, treatments)
  metainfo <- .compute_metainfo(data, 3 * 365)
  expect_equal(
    as.character(metainfo$GROUP), c(tte_outcomes, continuous_outcome))
  expect_equal(metainfo$n, c(129, 115, 110, 77, 569))
  expect_equal(metainfo$proportion, c(12.9, 11.5, 11, 7.7, 56.9))
  expect_equal(
    metainfo$maxday,
    c(1074.68287, 1068.22797, 1074.46617, 1028.40857, 63.76411),
    tol = 1e-5
  )
  expect_equal(metainfo$fixed.followup, c(1095, 1095, 1095, 1095, 1095))
  expect_equal(metainfo$startx, c(0, 12.9, 24.4, 35.4, 43.1))
  expect_equal(metainfo$endx, c(12.9, 24.4, 35.4, 43.1, 100))
  expect_equal(metainfo$starty, c(0, 0, 0, 0, 0))
  expect_equal(metainfo$n.groups, c(5, 5, 5, 5, 5))
  expect_equal(metainfo$n_Active, c(63, 55, 50, 34, 298))
  expect_equal(metainfo$n_Control, c(66, 60, 60, 43, 271))
})

test_that("Test compute survmod", {
  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  treatments <- c("Active", "Control")
  data <- .reformat_data(data, tte_outcomes, continuous_outcome, treatments)
  meta <- .compute_metainfo(data, 3 * 365)
  survmod <- .compute_survmod(
    data, meta, tte_outcomes, continuous_outcome, treatments)

  # Checking the abssum along the columns to check that values remain the same.
  expect_equal(sum(abs(survmod$data$time)), 1119088.643)
  expect_equal(sum(abs(survmod$data$n.risk)), 848310)
  expect_equal(sum(abs(survmod$data$n.event)), 560)
  expect_equal(sum(abs(survmod$data$n.censor)), 4440)
  expect_equal(sum(abs(survmod$data$surv)), 2026.4490767)
  expect_equal(sum(abs(survmod$data$std.err)), 25.26028588)
  expect_equal(sum(abs(survmod$data$upper)), 2072.29765)
  expect_equal(sum(abs(survmod$data$lower)), 1981.654951)
  expect_equal(sum(abs(survmod$data$adjusted.time)), 42925.486)
  expect_equal(sum(abs(survmod$data$km.y)), 138.5509233)
  expect_equal(sum(abs(survmod$data$max)), 29658.7019)
  expect_equal(sum(abs(survmod$data$sum.event)), 177583)
  expect_equal(sum(abs(survmod$data$km.start)), 38233.38498)
  expect_equal(sum(abs(survmod$data$km.end)), 67892.08688)

  expect_equal(
    survmod$meta$max,
    c(14.877758, 13.108431, 11.869606,  8.567655, 16.389072,
      14.545123, 14.965483, 10.791931), tol = 1e-6)
  expect_equal(survmod$meta$sum.event, c(126, 55, 50, 34, 132, 60, 60, 43))
  expect_equal(survmod$meta$km.start,
    c(0.00000, 14.87776, 27.98619, 39.85580, 0.00000,
      16.38907, 30.93419, 45.89968), tol = 1e-6

  )
  expect_equal(survmod$meta$km.end,
    c(14.87776, 27.98619, 39.85580, 48.42345, 16.38907,
      30.93419, 45.89968, 56.69161), tol = 1e-6
  )

})


test_that("Test compute slope", {
  file <- fixture_path("hce_scenario_a.csv")
  data <- read.csv(file)
  tte_outcomes <- c(
    "Outcome I", "Outcome II", "Outcome III", "Outcome IV"
  )
  continuous_outcome <- "Continuous outcome"
  treatments <- c("Active", "Control")
  data <- .reformat_data(data, tte_outcomes, continuous_outcome, treatments)
  meta <- .compute_metainfo(data, 3 * 365)
  survmod <- .compute_survmod(
    data, meta, tte_outcomes, continuous_outcome, treatments)
  slope <- .compute_slope(
    data, meta, survmod, tte_outcomes, continuous_outcome, treatments)
  expect_equal(sum(abs(slope$data$x)), 40828.387)
  expect_equal(sum(abs(slope$data$violinx)), 40711.95)
  expect_equal(sum(abs(slope$data$violiny)), 29793.61428)

  expect_equal(slope$meta$n, c(298, 271))
  expect_equal(slope$meta$median, c(74.360287, 68.354528))
  expect_equal(slope$meta$average, c(73.72377, 69.5893123))
})
