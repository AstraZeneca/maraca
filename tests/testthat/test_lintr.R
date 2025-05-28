test_that("linting", {
  skip_on_covr()
  lints <- lintr::lint_dir("../../R",
                           linters  = lintr::linters_with_defaults(
                             object_name_linter = NULL,
                             return_linter = NULL
                           ))
  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
  } else {
    succeed()
  }

  lints <- lintr::lint_dir("../../tests",
                           linters  = lintr::linters_with_defaults(
                             object_name_linter = NULL,
                             return_linter = NULL
                           ))
  if (length(lints) != 0) {
    print("")
    print(lints)
    fail("Linting failed")
  } else {
    succeed()
  }
})
