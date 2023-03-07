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

unlink(artifacts_path(), recursive = TRUE)
if (!dir.exists(artifacts_path())) {
  dir.create(artifacts_path(), recursive = TRUE)
}
