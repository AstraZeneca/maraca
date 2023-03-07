test_that("linting", {
  skip_on_covr()
  lints <- lintr::lint_dir("../../R")
  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
  } else {
    succeed()
  }

  lints <- lintr::lint_dir("../../tests")
  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
  } else {
    succeed()
  }
})
