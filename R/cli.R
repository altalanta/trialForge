#' Command line interface for trialforgeR
#'
#' Parses command line arguments, runs validations, and generates a QC report.
#'
#' @param args Character vector of command line arguments. Defaults to the
#'   trailing command line arguments.
#'
#' @export
trialforge_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  opts <- .parse_cli_args(args)

  if (length(opts$data_files) == 0) {
    cli::cli_abort("Provide at least one dataset using --sdtm_dm, --sdtm_ae, or --adam_adae.")
  }

  spec_path <- opts$spec %||% system.file("extdata/specs/validation_spec.csv", package = "trialforgeR")
  results <- trialforge_validate(opts$data_files, spec_path = spec_path)
  report_path <- generate_qc_report(results, out_file = opts$out %||% "qc_report.html")

  cli::cli_inform(c("v" = "Validation complete. Report written to {.path {report_path}}"))
  invisible(list(results = results, report = report_path))
}

.parse_cli_args <- function(args) {
  data_files <- list()
  spec <- NULL
  out <- NULL

  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    next_value <- function() {
      if (i + 1 > length(args)) {
        cli::cli_abort("Flag {.val {arg}} requires a value.")
      }
      value <- args[[i + 1]]
      i <<- i + 1
      value
    }

    switch(arg,
           "--spec" = {
             spec <- next_value()
           },
           "--sdtm_dm" = {
             data_files$sdtm_dm <- next_value()
           },
           "--sdtm_ae" = {
             data_files$sdtm_ae <- next_value()
           },
           "--adam_adae" = {
             data_files$adam_adae <- next_value()
           },
           "--out" = {
             out <- next_value()
           },
           "--help" = {
             .print_cli_help()
             return(list(data_files = list(), spec = NULL, out = NULL))
           },
           {
             cli::cli_abort("Unknown argument {.val {arg}}. Use --help for usage.")
           }
    )
    i <- i + 1
  }

  list(data_files = data_files, spec = spec, out = out)
}

.print_cli_help <- function() {
  message(
    "trialforge_cli usage:\n",
    "  --spec <path>        Path to validation specification CSV (optional)\n",
    "  --sdtm_dm <path>     Path to SDTM DM dataset (CSV)\n",
    "  --sdtm_ae <path>     Path to SDTM AE dataset (CSV)\n",
    "  --adam_adae <path>   Path to ADaM ADAE dataset (CSV)\n",
    "  --out <path>         Output QC report path (optional, default qc_report.html)\n",
    "  --help               Display this message\n"
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
