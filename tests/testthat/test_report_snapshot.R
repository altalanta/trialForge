testthat::test_that("generate_qc_report writes HTML file", {
  results <- tibble::tibble(
    domain = c("sdtm_dm", "sdtm_dm"),
    check = c("REQUIRED::USUBJID", "SDTM_DM::USUBJID_UNIQUENESS"),
    status = c("PASS", "FAIL"),
    details = c("present", "duplicates found")
  )
  tmp <- tempfile(fileext = ".html")
  path <- generate_qc_report(results, out_file = tmp)
  testthat::expect_true(file.exists(tmp))
  testthat::expect_equal(normalizePath(path, winslash = "/", mustWork = TRUE), normalizePath(tmp, winslash = "/", mustWork = TRUE))
})
