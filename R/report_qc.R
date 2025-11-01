#' Generate a QC HTML report
#'
#' Renders a minimal HTML report summarising validation results.
#'
#' @param results Tibble of validation results containing at least columns
#'   `domain`, `check`, `status`, and `details`.
#' @param out_file Output file path. Defaults to `qc_report.html` in the
#'   working directory.
#'
#' @return Invisibly returns the absolute path to the generated report.
#' @export
generate_qc_report <- function(results, out_file = "qc_report.html") {
  stopifnot(is.data.frame(results))

  output_dir <- fs::path_abs(fs::path_dir(out_file))
  fs::dir_create(output_dir)
  output_path <- fs::path_abs(out_file)

  template <- c(
    "---",
    "title: \"trialforgeR Validation Report\"",
    "output:",
    "  html_document:",
    "    theme: flatly",
    "    toc: false",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(dplyr)",
    "library(knitr)",
    "library(DT)",
    "options(datatable.print.keys = FALSE)",
    "```",
    "",
    "## Validation Summary",
    "",
    "```{r}",
    "knitr::kable(summary_tbl, caption = \"Validation results by status\")",
    "```",
    "",
    "```{r}",
    "jsonlite::prettify(summary_json)",
    "```",
    "",
    "```{r}",
    "cat(summary_yaml)",
    "```",
    "",
    "## Validation Details",
    "",
    "```{r, results='asis'}",
    "DT::datatable(results, options = list(pageLength = 20))",
    "```",
    "",
    "## Validation Table",
    "",
    "```{r}",
    "knitr::kable(results, format = \"html\", caption = \"Validation result table\")",
    "```"
  )

  temp_rmd <- tempfile("trialforge_report_", fileext = ".Rmd")
  writeLines(template, temp_rmd)

  render_env <- new.env(parent = globalenv())
  render_env$results <- tibble::as_tibble(results)
  summary_tbl <- dplyr::count(render_env$results, status, name = "n", sort = TRUE)
  render_env$summary_tbl <- summary_tbl
  render_env$summary_json <- jsonlite::toJSON(summary_tbl, pretty = TRUE, auto_unbox = TRUE)
  render_env$summary_yaml <- yaml::as.yaml(stats::setNames(as.list(summary_tbl$n), summary_tbl$status))

  rmarkdown::render(
    input = temp_rmd,
    output_file = fs::path_file(output_path),
    output_dir = output_dir,
    quiet = TRUE,
    envir = render_env
  )

  invisible(output_path)
}

#' Run trialforgeR validations
#'
#' @param data_files Named list mapping domain identifiers to CSV file paths.
#'   Supported domains include `sdtm_dm`, `sdtm_ae`, and `adam_adae`.
#' @param spec_path Path to the validation spec CSV.
#'
#' @return Tibble of validation results sorted by domain and check.
#' @export
trialforge_validate <- function(
    data_files,
    spec_path = system.file("extdata/specs/validation_spec.csv", package = "trialforgeR")
) {
  if (!rlang::is_named(data_files)) {
    cli::cli_abort("`data_files` must be a named list of domain file paths.")
  }

  spec <- read_validation_spec(spec_path)

  purrr::imap_dfr(
    data_files,
    function(path, domain_name) {
      if (!fs::file_exists(path)) {
        cli::cli_abort("File {.path {path}} for domain {.val {domain_name}} does not exist.")
      }

      cli::cli_inform(c("i" = "Validating domain {.val {domain_name}} from {.path {path}}"))
      df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
      domain_spec <- dplyr::filter(spec, domain == domain_name)

      core_results <- tibble::as_tibble(run_core_validations(df, domain_spec)) %>%
        dplyr::mutate(domain = domain_name, .before = 1L)

      domain_checks <- .run_domain_validations(domain_name, df)
      if (nrow(domain_checks) > 0) {
        domain_checks <- dplyr::mutate(domain_checks, domain = domain_name, .before = 1L)
      }

      dplyr::bind_rows(core_results, domain_checks)
    }
  ) %>%
    dplyr::arrange(domain, check)
}

.run_domain_validations <- function(domain, df) {
  switch(domain,
         sdtm_dm = check_sdtm_dm_unique_usubjid(df),
         sdtm_ae = check_sdtm_ae_sev_values(df),
         tibble::tibble(check = character(), status = character(), details = character()))
}
