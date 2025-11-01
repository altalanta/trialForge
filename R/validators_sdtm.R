#' Check SDTM DM unique USUBJID
#'
#' Ensures the DM domain contains a unique, non-missing `USUBJID` for all
#' records.
#'
#' @param dm Data frame representing SDTM DM.
#'
#' @return Tibble with columns `check`, `status`, and `details`.
#' @export
check_sdtm_dm_unique_usubjid <- function(dm) {
  if (!"USUBJID" %in% names(dm)) {
    return(tibble::tibble(
      check = "SDTM_DM::USUBJID_UNIQUENESS",
      status = "FAIL",
      details = "USUBJID column missing from DM dataset."
    ))
  }

  missing_count <- sum(is.na(dm$USUBJID) | dm$USUBJID == "")
  duplicate_count <- sum(duplicated(dm$USUBJID[!is.na(dm$USUBJID) & dm$USUBJID != ""]))

  if (missing_count == 0 && duplicate_count == 0) {
    return(tibble::tibble(
      check = "SDTM_DM::USUBJID_UNIQUENESS",
      status = "PASS",
      details = "All USUBJID values are present and unique."
    ))
  }

  reasons <- c()
  if (missing_count > 0) {
    reasons <- c(reasons, glue::glue("{missing_count} missing USUBJID"))
  }
  if (duplicate_count > 0) {
    reasons <- c(reasons, glue::glue("{duplicate_count} duplicate USUBJID"))
  }

  tibble::tibble(
    check = "SDTM_DM::USUBJID_UNIQUENESS",
    status = "FAIL",
    details = glue::glue("USUBJID issues detected: {paste(reasons, collapse = '; ')}.")
  )
}

#' Check SDTM AE severity values
#'
#' Verifies that the AE domain `AESEV` values are within the allowed set.
#'
#' @param ae Data frame representing SDTM AE.
#'
#' @return Tibble with validation outcome.
#' @export
check_sdtm_ae_sev_values <- function(ae) {
  if (!"AESEV" %in% names(ae)) {
    return(tibble::tibble(
      check = "SDTM_AE::AESEV_VALUES",
      status = "FAIL",
      details = "AESEV column missing from AE dataset."
    ))
  }

  allowed <- c("MILD", "MODERATE", "SEVERE")
  values <- ae$AESEV
  invalid <- unique(stats::na.omit(values[!values %in% allowed]))

  if (length(invalid) == 0) {
    tibble::tibble(
      check = "SDTM_AE::AESEV_VALUES",
      status = "PASS",
      details = "All AESEV values meet allowed set."
    )
  } else {
    tibble::tibble(
      check = "SDTM_AE::AESEV_VALUES",
      status = "FAIL",
      details = glue::glue("Invalid AESEV values: {paste(invalid, collapse = ', ')}")
    )
  }
}
