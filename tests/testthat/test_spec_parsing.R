testthat::test_that("validation spec has expected structure", {
  spec <- read_validation_spec()
  testthat::expect_s3_class(spec, "tbl_df")
  testthat::expect_true(all(c("domain", "var", "type", "required", "allowed_values", "regex") %in% names(spec)))
  testthat::expect_type(spec$required, "logical")
})

testthat::test_that("allowed values are parsed into list columns", {
  spec <- read_validation_spec()
  ae_row <- spec[spec$var == "AESEV", ][["allowed_values"]][[1]]
  testthat::expect_equal(ae_row, c("MILD", "MODERATE", "SEVERE"))
})
