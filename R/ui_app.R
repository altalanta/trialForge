#' Launch the trialforgeR Shiny app
#'
#' Provides a minimal interface to run validations against uploaded files and
#' download the resulting QC report.
#'
#' @export
run_trialforge_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("trialforgeR Validation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("spec", "Validation Spec (CSV)", accept = ".csv"),
        shiny::fileInput("dm", "SDTM DM (CSV)", accept = ".csv"),
        shiny::fileInput("ae", "SDTM AE (CSV)", accept = ".csv"),
        shiny::fileInput("adae", "ADaM ADAE (CSV)", accept = ".csv"),
        shiny::actionButton("run", "Run Validation", class = "btn-primary"),
        shiny::br(),
        shiny::downloadButton("download_report", "Download QC Report")
      ),
      shiny::mainPanel(
        shiny::h3("Validation Results"),
        DT::DTOutput("results_table"),
        shiny::verbatimTextOutput("message")
      )
    )
  )

  server <- function(input, output, session) {
    results_rv <- shiny::reactiveVal(NULL)

    observe_event <- function(id, domain) {
      shiny::observeEvent(input[[id]], {
        if (!is.null(input[[id]])) {
          cli::cli_inform(c("i" = "Uploaded {.val {domain}} file: {.path {input[[id]]$datapath}}"))
        }
      })
    }

    observe_event("dm", "sdtm_dm")
    observe_event("ae", "sdtm_ae")
    observe_event("adae", "adam_adae")

    shiny::observeEvent(input$run, {
      tryCatch(
        {
          data_files <- list()
          if (!is.null(input$dm)) data_files$sdtm_dm <- input$dm$datapath
          if (!is.null(input$ae)) data_files$sdtm_ae <- input$ae$datapath
          if (!is.null(input$adae)) data_files$adam_adae <- input$adae$datapath

          if (length(data_files) == 0) {
            stop("Upload at least one dataset before running validation.")
          }

          spec_path <- if (!is.null(input$spec)) input$spec$datapath else
            system.file("extdata/specs/validation_spec.csv", package = "trialforgeR")

          results <- trialforge_validate(data_files, spec_path = spec_path)
          results_rv(results)
          output$message <- shiny::renderText("Validation completed successfully.")
        },
        error = function(err) {
          results_rv(NULL)
          output$message <- shiny::renderText(glue::glue("Validation failed: {err$message}"))
        }
      )
    })

    output$results_table <- DT::renderDT({
      res <- results_rv()
      if (is.null(res)) {
        return(NULL)
      }
      DT::datatable(res, options = list(pageLength = 20))
    })

    output$download_report <- shiny::downloadHandler(
      filename = function() {
        glue::glue("trialforge_report_{format(Sys.time(), '%Y%m%d_%H%M%S')}.html")
      },
      content = function(file) {
        res <- results_rv()
        if (is.null(res)) {
          stop("Run validation before downloading the report.")
        }
        generate_qc_report(res, out_file = file)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
