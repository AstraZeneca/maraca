test_that("Maraca initialisation", {
  file <- fixture_path("hce_scenario_a.csv")
  mar <- maraca(file)
  expect_true(TRUE)
  #expect_s3_class(mar, "maraca::maraca")
  #plot(mar)
  #print(plot_tte_trellis(mar))
})
