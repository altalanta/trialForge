#' Validate presence of required variables
#'
#' @param df A data frame or tibble to validate.
#' @param spec Specification tibble filtered for the domain of interest.
#'
#' @return A tibble with columns `check`, `status`, and `details`.
#' @export
validate_required_vars <- function(df, spec) {
  present_vars <- names(df)

  purrr::map_dfr(
    seq_len(nrow(spec)),
    function(idx) {
      row <- spec[idx, ]
      check_id <- glue::glue("REQUIRED::{row$var}")

      if (!isTRUE(row$required)) {
        return(tibble::tibble(
          check = check_id,
          status = "SKIP",
          details = "Variable not marked as required."
        ))
      }

      if (!row$var %in% present_vars) {
        return(tibble::tibble(
          check = check_id,
          status = "FAIL",
          details = "Required variable is missing from dataset."
        ))
      }

      column <- df[[row$var]]
      if (all(is.na(column))) {
        return(tibble::tibble(
          check = check_id,
          status = "FAIL",
          details = "Required variable contains only missing values."
        ))
      }

      tibble::tibble(
        check = check_id,
        status = "PASS",
        details = "Required variable present with non-missing values."
      )
    }
  )
}

#' Validate variable types
#'
#' @inheritParams validate_required_vars
#'
#' @return A tibble summarising type validation results.
#' @export
validate_types <- function(df, spec) {
  purrr::map_dfr(
    seq_len(nrow(spec)),
    function(idx) {
      row <- spec[idx, ]
      expected_type <- row$type
      check_id <- glue::glue("TYPE::{row$var}")

      if (!row$var %in% names(df)) {
        return(tibble::tibble(
          check = check_id,
          status = "SKIP",
          details = "Variable not present; type validation skipped."
        ))
      }

      column <- df[[row$var]]
      status <- .check_column_type(column, expected_type)
      tibble::tibble(
        check = check_id,
        status = status$status,
        details = status$details
      )
    }
  )
}

#' Validate value sets
#'
#' @inheritParams validate_required_vars
#'
#' @return A tibble summarising value-set validation results.
#' @export
validate_value_sets <- function(df, spec) {
  purrr::map_dfr(
    seq_len(nrow(spec)),
    function(idx) {
      row <- spec[idx, ]
      check_id <- glue::glue("VALUESET::{row$var}")
      allowed <- row$allowed_values[[1]]

      if (is.null(allowed) || length(allowed) == 0) {
        return(tibble::tibble(
          check = check_id,
          status = "SKIP",
          details = "No value set defined."
        ))
      }

      if (!row$var %in% names(df)) {
        return(tibble::tibble(
          check = check_id,
          status = "SKIP",
          details = "Variable not present; value-set validation skipped."
        ))
      }

      values <- df[[row$var]]
      invalid <- unique(stats::na.omit(values[!values %in% allowed]))

      if (length(invalid) == 0) {
        tibble::tibble(
          check = check_id,
          status = "PASS",
          details = "All values fall within the allowed set."
        )
      } else {
        tibble::tibble(
          check = check_id,
          status = "FAIL",
          details = glue::glue("Invalid values detected: {paste(invalid, collapse = ', ')}")
        )
      }
    }
  )
}

#' Run core validations
#'
#' @inheritParams validate_required_vars
#'
#' @return A tibble combining required variable, type, and value-set check
#'   results.
#' @export
run_core_validations <- function(df, spec) {
  dplyr::bind_rows(
    validate_required_vars(df, spec),
    validate_types(df, spec),
    validate_value_sets(df, spec)
  ) %>%
    tibble::as_tibble()
}

.check_column_type <- function(column, expected_type) {
  non_missing <- column[!is.na(column)]

  if (length(non_missing) == 0) {
    return(list(
      status = "PASS",
      details = "All values missing; type validation passes by default."
    ))
  }

  checkers <- list(
    character = function(x) is.character(x) || is.factor(x),
    numeric = function(x) is.numeric(x),
    integer = function(x) is.integer(x) || (is.numeric(x) && all(abs(x - round(x)) < .Machine$double.eps^0.5)),
    date = function(x) inherits(x, "Date") || is.character(x)
  )

  if (!expected_type %in% names(checkers)) {
    return(list(
      status = "SKIP",
      details = glue::glue("Type {expected_type} not implemented.")
    ))
  }

  valid <- checkers[[expected_type]](non_missing)
  if (isTRUE(valid)) {
    list(status = "PASS", details = glue::glue("Column matches expected type {expected_type}."))
  } else {
    list(status = "FAIL", details = glue::glue("Column does not match expected type {expected_type}."))
  }
}
