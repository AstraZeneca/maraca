expect_file_exists <- function(path) {
  expect_true(file.exists(path))
}

expect_file_not_exists <- function(path) {
  expect_false(file.exists(path))
}

pdf_device <- NULL
set_pdf_output <- function(path) {
  cleanup_pdf_output()
  pdf(file = path)
  pdf_device <<- dev.cur()
}

cleanup_pdf_output <- function() {
  if (!is.null(pdf_device)) {
    dev.off(pdf_device)
  }
}
withr::defer(cleanup_pdf_output(), teardown_env())
