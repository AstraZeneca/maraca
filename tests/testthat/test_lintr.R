test_that("Linting R source", {
  skip_on_covr()
  lints <- lintr::lint_dir("../../R")
  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
  } else {
    succeed()
  }
})

test_that("Linting tests", {
  skip_on_covr()
  lints <- lintr::lint_dir("../../tests")
  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
  } else {
    succeed()
  }
})
