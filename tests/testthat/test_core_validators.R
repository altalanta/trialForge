testthat::test_that("required variable validation passes and fails appropriately", {
  spec <- read_validation_spec() |> dplyr::filter(domain == "sdtm_dm")
  df_ok <- tibble::tibble(USUBJID = c("A", "B"), SEX = c("M", "F"))
  res_ok <- validate_required_vars(df_ok, spec)
  row_usubjid <- res_ok[res_ok$check == "REQUIRED::USUBJID", , drop = FALSE]
  testthat::expect_equal(row_usubjid$status, "PASS")

  df_missing <- tibble::tibble(SEX = c("M", "F"))
  res_missing <- validate_required_vars(df_missing, spec)
  row_missing <- res_missing[res_missing$check == "REQUIRED::USUBJID", , drop = FALSE]
  testthat::expect_equal(row_missing$status, "FAIL")
})

testthat::test_that("type validation detects mismatches", {
  spec_num <- tibble::tibble(
    domain = "x",
    var = "AGE",
    type = "numeric",
    required = FALSE,
    allowed_values = list(NULL),
    regex = NA_character_
  )

  df_good <- tibble::tibble(AGE = c(30.5, 40))
  res_good <- validate_types(df_good, spec_num)
  testthat::expect_equal(res_good$status, "PASS")

  df_bad <- tibble::tibble(AGE = c("30", "40"))
  res_bad <- validate_types(df_bad, spec_num)
  testthat::expect_equal(res_bad$status, "FAIL")
})
