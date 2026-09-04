mod_charts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tags$div(
    class = "chart-grid",
    htmltools::tags$section(
      class = "viz-card viz-card-wide",
      role = "img",
      `aria-label` = "Série temporal dos valores no período selecionado",
      shiny::plotOutput(
        ns("series_plot"),
        height = "420px"
      )
    ),
    htmltools::tags$section(
      class = "viz-card viz-card-wide",
      role = "img",
      `aria-label` = "Ranking de condições, procedimentos ou territórios",
      shiny::uiOutput(ns("ranking_ui"))
    ),
    htmltools::tags$section(
      class = "viz-card viz-card-wide",
      shiny::uiOutput(ns("comparison_ui"))
    ),
    htmltools::tags$details(
      class = "chart-data-table",
      htmltools::tags$summary("Ver dados equivalentes aos gráficos"),
      shiny::tableOutput(ns("series_table")),
      shiny::tableOutput(ns("ranking_table"))
    )
  )
}

mod_charts_server <- function(id, analysis) {
  shiny::moduleServer(id, function(input, output, session) {
    compact_plot <- function(output_id) {
      keys <- c(
        paste0("output_", output_id, "_width"),
        paste0("output_", session$ns(output_id), "_width")
      )
      widths <- vapply(keys, function(key) {
        value <- session$clientData[[key]]
        if (is.null(value)) NA_real_ else as.numeric(value)
      }, numeric(1))
      widths <- widths[is.finite(widths)]
      width <- if (length(widths) > 0L) widths[[1L]] else 900
      width < 600
    }

    output$ranking_ui <- shiny::renderUI({
      value <- require_analysis_value(analysis())
      rows <- min(as.integer(value$query$top_n %||% 15L), nrow(value$ranking))
      labels <- utils::head(value$ranking$label, rows)
      line_count <- stringr::str_count(stringr::str_wrap(labels, width = 42L), "\n") + 1L
      height <- max(440L, sum(28L + 18L * (line_count - 1L)) + 170L)
      shiny::plotOutput(
        session$ns("ranking_plot"),
        height = paste0(height, "px")
      )
    })

    output$comparison_ui <- shiny::renderUI({
      value <- require_analysis_value(analysis())
      if (nrow(value$comparisons) == 0L) return(NULL)
      rows <- min(15L, nrow(value$comparisons))
      labels <- utils::head(value$comparisons$geo_name, rows)
      line_count <- stringr::str_count(stringr::str_wrap(labels, width = 38L), "\n") + 1L
      height <- max(480L, sum(28L + 18L * (line_count - 1L)) + 170L)
      shiny::plotOutput(
        session$ns("comparison_plot"), height = paste0(height, "px")
      )
    })

    output$series_plot <- shiny::renderPlot({
      value <- require_analysis_value(analysis())
      shiny::validate(shiny::need(nrow(value$series) > 0L, "Série temporal indisponível."))
      make_series_plot(value, compact = compact_plot("series_plot"))
    }, res = 144)

    output$ranking_plot <- shiny::renderPlot({
      value <- require_analysis_value(analysis())
      shiny::validate(shiny::need(nrow(value$ranking) > 0L, "Ranking indisponível."))
      make_ranking_plot(value, compact = compact_plot("ranking_plot"))
    }, res = 144)

    output$comparison_plot <- shiny::renderPlot({
      value <- require_analysis_value(analysis())
      plot <- make_comparison_plot(value, compact = compact_plot("comparison_plot"))
      shiny::validate(shiny::need(!is.null(plot), "Comparação indisponível."))
      plot
    }, res = 144)

    output$series_table <- shiny::renderTable({
      value <- require_analysis_value(analysis())
      utils::head(export_series_table(value), 24L)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")

    output$ranking_table <- shiny::renderTable({
      value <- require_analysis_value(analysis())
      utils::head(export_ranking_table(value), as.integer(value$query$top_n %||% 15L))
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
  })
}
