#' Validate using a Python helper
#'
#' Demonstrates an optional Python bridge using `reticulate`. The function is
#' intentionally lightweight and is not invoked during automated testing.
#'
#' @param df Data frame to validate.
#' @param script_path Path to a Python script defining a `validate(data)` function.
#'
#' @return The object returned by the Python `validate` function, or `NULL` if
#'   the bridge could not be executed.
#' @export
validate_with_python <- function(df, script_path) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_warn("reticulate not available; skipping Python validation.")
    return(NULL)
  }

  if (!fs::file_exists(script_path)) {
    cli::cli_warn("Python script {.path {script_path}} not found.")
    return(NULL)
  }

  reticulate::source_python(script_path, envir = new.env(parent = globalenv()))

  if (!reticulate::py_has_attr(reticulate::py, "validate")) {
    cli::cli_warn("Python script does not expose a `validate` function.")
    return(NULL)
  }

  py_data <- reticulate::r_to_py(tibble::as_tibble(df))
  reticulate::py$validate(py_data)
}
