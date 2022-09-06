root_path <- function(...) {
  here::here(...)
}

fixture_path <- function(...) {
  file.path(root_path(), "tests", "fixtures", ...)
}

cached_path <- function(...) {
  file.path(root_path(), "tests", "cached_calculations", ...)
}

artifacts_path <- function(...) {
  file.path(root_path(), "tests", "artifacts", ...)
}

if (!dir.exists(artifacts_path())) {
  dir.create(artifacts_path(), recursive = TRUE)
}

pdf_device <- NULL
if (generates_plots) {
  pdf(file = artifacts_path("test_plots.pdf"))
  pdf_device <- dev.cur()
}

cleanup <- function() {
  if (!is.null(pdf_device)) {
    dev.off(pdf_device)
  }
}
withr::defer(cleanup(), teardown_env())
