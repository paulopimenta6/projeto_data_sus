metric_value_text <- function(value, metric = "count") {
  if (is.na(value)) return("Sem dado")
  format_pt_number(value, accuracy = if (metric == "rate") 0.1 else 1)
}

kpi_card <- function(label, value, note = NULL, accent = "teal") {
  htmltools::tags$article(
    class = paste("kpi-card", paste0("kpi-", accent)),
    htmltools::tags$p(class = "kpi-label", label),
    htmltools::tags$p(class = "kpi-value", value),
    if (!is.null(note)) htmltools::tags$p(class = "kpi-note", note)
  )
}

mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("query_heading")),
    shiny::uiOutput(ns("kpis")),
    shiny::uiOutput(ns("warnings"))
  )
}

mod_summary_server <- function(id, analysis) {
  shiny::moduleServer(id, function(input, output, session) {
    output$query_heading <- shiny::renderUI({
      value <- require_analysis_value(analysis())
      htmltools::tags$div(
        class = "analysis-heading",
        htmltools::tags$div(
          htmltools::tags$p(class = "eyebrow", "RESULTADO ATUAL"),
          htmltools::tags$h2(value$query$domain_label),
          htmltools::tags$p(
            paste0(
              value$query$measure_label, " · ",
              paste(value$query$periods$id, collapse = ", "), " · ",
              value$query$uf %||% "Brasil"
            )
          )
        ),
        htmltools::tags$span(
          class = "source-chip",
          paste0("Geometria ", value$geometry_year %||% "indisponível")
        )
      )
    })

    output$kpis <- shiny::renderUI({
      value <- require_analysis_value(analysis())
      summary <- value$summary
      second_label <- if (summary$metric == "rate") "Taxa geral" else "Competências"
      second_value <- if (summary$metric == "rate") {
        metric_value_text(summary$overall_rate, "rate")
      } else {
        as.character(summary$periods)
      }
      second_note <- if (summary$metric == "rate") "por 100 mil pessoas-ano" else summary$latest_period

      htmltools::tags$div(
        class = "kpi-grid",
        kpi_card("Total registrado", metric_value_text(summary$total), value$query$measure_label, "navy"),
        kpi_card(second_label, second_value, second_note, "gold"),
        kpi_card(
          "Maior valor territorial",
          summary$top_name,
          metric_value_text(summary$top_value, summary$metric),
          "teal"
        ),
        kpi_card(
          "Territórios com dados",
          format_pt_number(summary$territories_with_data),
          value$query$geo_level,
          "coral"
        )
      )
    })

    output$warnings <- shiny::renderUI({
      value <- analysis()
      if (is.null(value)) return(NULL)
      if (inherits(value, "datasus_analysis_error")) {
        return(htmltools::tags$div(
          class = "analysis-alert alert-error",
          htmltools::tags$strong("A consulta não foi concluída."),
          htmltools::tags$p(value$message)
        ))
      }
      if (length(value$warnings) == 0L) return(NULL)
      htmltools::tags$details(
        class = "analysis-alert alert-warning",
        htmltools::tags$summary(paste(length(value$warnings), "aviso(s) metodológico(s)")),
        htmltools::tags$ul(lapply(value$warnings, htmltools::tags$li))
      )
    })
  })
}
