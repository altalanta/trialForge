testthat::test_that("duplicate USUBJID fails DM uniqueness check", {
  dm <- tibble::tibble(USUBJID = c("A", "B", "A"))
  res <- check_sdtm_dm_unique_usubjid(dm)
  testthat::expect_equal(res$status, "FAIL")
})

testthat::test_that("valid AE severity values pass", {
  ae <- tibble::tibble(AESEV = c("MILD", "SEVERE", NA))
  res <- check_sdtm_ae_sev_values(ae)
  testthat::expect_equal(res$status, "PASS")
})

testthat::test_that("invalid AE severity values fail", {
  ae <- tibble::tibble(AESEV = c("CRITICAL", "MILD"))
  res <- check_sdtm_ae_sev_values(ae)
  testthat::expect_equal(res$status, "FAIL")
})
