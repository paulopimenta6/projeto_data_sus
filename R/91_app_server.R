app_server <- function(input, output, session) {
  filter_state <- mod_filters_server("filters")
  analysis <- shiny::reactiveVal(NULL)

  shiny::observeEvent(filter_state$analyze_trigger(), {
    result <- shiny::withProgress(message = "Consultando e analisando dados do SUS...", value = 0.1, {
      result <- tryCatch(
        {
          query <- filter_state$query()
          shiny::incProgress(0.2, detail = "Baixando e agregando registros públicos")
          value <- run_analysis(query)
          shiny::incProgress(0.7, detail = "Preparando indicadores e mapas")
          value
        },
        error = analysis_error
      )
      result
    })
    analysis(result)
  }, ignoreInit = TRUE)

  output$app_status <- shiny::renderUI({
    value <- analysis()
    if (is.null(value)) {
      return(htmltools::tags$div(
        class = "app-status status-idle",
        htmltools::tags$span(class = "status-dot"),
        "Aguardando consulta"
      ))
    }
    if (inherits(value, "datasus_analysis_error")) {
      return(htmltools::tags$div(class = "app-status status-error", "Falha na última consulta"))
    }
    htmltools::tags$div(
      class = "app-status status-ok",
      htmltools::tags$span(class = "status-dot"),
      paste("Consulta concluída ·", format(Sys.time(), "%H:%M"))
    )
  })

  mod_summary_server("summary", analysis)
  mod_charts_server("charts", analysis)
  facilities <- mod_maps_server("maps", analysis)
  mod_data_exports_server("data_exports", analysis, facilities)
}
