test_that("Maraca initialisation", {
  file <- fixture_path("HCE scenario A.csv")
  mar <- maraca(file)
  expect_s3_class(mar, "maraca::maraca")
  plot(mar)
  print(plot_tte_trellis(mar))
})
