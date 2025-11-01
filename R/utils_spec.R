#' Read the validation specification
#'
#' Reads a CSV-formatted specification describing validation rules for each
#' domain and variable. The specification is expected to contain the columns
#' `domain`, `var`, `type`, `required`, `allowed_values`, and `regex`.
#'
#' @param path Path to the CSV specification file. Defaults to the packaged
#'   validation specification installed with the package.
#'
#' @return A tibble with the specification data where `required` is logical and
#'   `allowed_values` is a list column of character vectors (or `NULL` when no
#'   constraints are supplied).
#' @export
read_validation_spec <- function(
    path = system.file("extdata/specs/validation_spec.csv", package = "trialforgeR")
) {
  cli::cli_inform(c("i" = "Reading validation specification from {.path {path}}"))

  spec <- readr::read_csv(
    file = path,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(
      domain = readr::col_character(),
      var = readr::col_character(),
      type = readr::col_character(),
      required = readr::col_logical(),
      allowed_values = readr::col_character(),
      regex = readr::col_character()
    )
  )

  spec <- spec %>%
    tidyr::replace_na(list(allowed_values = "", regex = "")) %>%
    dplyr::mutate(
      required = as.logical(required),
      allowed_values = purrr::map(
        allowed_values,
        ~ {
          if (is.na(.x) || identical(.x, "")) {
            return(NULL)
          }
          stringr::str_split(.x, "\\|", simplify = FALSE)[[1]] %>%
            unique()
        }
      ),
      regex = dplyr::if_else(is.na(regex) | regex == "", NA_character_, regex)
    )

  tibble::as_tibble(spec)
}

#' Load packaged sample data
#'
#' Convenience helper to load synthetic SDTM or ADaM sample datasets packaged
#' with trialforgeR.
#'
#' @param domain Domain name. One of `"sdtm_dm"`, `"sdtm_ae"`, or `"adam_adae"`.
#'
#' @return A tibble containing the requested sample data.
#' @export
load_sample <- function(domain = c("sdtm_dm", "sdtm_ae", "adam_adae")) {
  domain <- rlang::arg_match(domain)
  file_path <- system.file(
    "extdata", "sample_data",
    glue::glue("{domain}.csv"),
    package = "trialforgeR"
  )
  if (file_path == "") {
    cli::cli_abort("Sample data for domain {.val {domain}} not found.")
  }
  readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE) %>%
    tibble::as_tibble()
}
