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
        height = "360px"
      )
    ),
    htmltools::tags$section(
      class = "viz-card viz-card-wide",
      role = "img",
      `aria-label` = "Ranking de condições, procedimentos ou territórios",
      shiny::plotOutput(
        ns("ranking_plot"),
        height = "480px"
      )
    )
  )
}

mod_charts_server <- function(id, analysis) {
  shiny::moduleServer(id, function(input, output, session) {
    output$series_plot <- shiny::renderPlot({
      value <- require_analysis_value(analysis())
      shiny::validate(shiny::need(nrow(value$series) > 0L, "Série temporal indisponível."))
      make_series_plot(value)
    }, res = 110)

    output$ranking_plot <- shiny::renderPlot({
      value <- require_analysis_value(analysis())
      shiny::validate(shiny::need(nrow(value$ranking) > 0L, "Ranking indisponível."))
      make_ranking_plot(value)
    }, res = 110)
  })
}
