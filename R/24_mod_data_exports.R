mod_data_exports_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::tags$section(
      class = "data-card",
      htmltools::tags$div(
        class = "section-heading",
        htmltools::tags$div(
          htmltools::tags$p(class = "eyebrow", "DADOS ANALÍTICOS"),
          htmltools::tags$h2("Tabela territorial")
        ),
        htmltools::tags$p("A tabela exibe o total e, quando disponível, denominador e taxa bruta.")
      ),
      DT::DTOutput(ns("table"))
    ),
    htmltools::tags$section(
      class = "export-card",
      htmltools::tags$div(
        class = "section-heading",
        htmltools::tags$div(
          htmltools::tags$p(class = "eyebrow", "REPRODUZIBILIDADE"),
          htmltools::tags$h2("Exportar resultados")
        )
      ),
      htmltools::tags$div(
        class = "export-grid",
        shiny::downloadButton(ns("map_csv"), "Territórios · CSV"),
        shiny::downloadButton(ns("series_csv"), "Série · CSV"),
        shiny::downloadButton(ns("ranking_csv"), "Ranking · CSV"),
        shiny::downloadButton(ns("geojson"), "Mapa · GeoJSON"),
        shiny::downloadButton(ns("series_png"), "Série · PNG"),
        shiny::downloadButton(ns("ranking_png"), "Ranking · PNG"),
        shiny::downloadButton(ns("map_png"), "Mapa · PNG"),
        shiny::downloadButton(ns("map_html"), "Mapa · HTML"),
        shiny::downloadButton(ns("facilities_geojson"), "Estabelecimentos · GeoJSON"),
        shiny::downloadButton(ns("manifest"), "Manifesto · JSON")
      )
    )
  )
}

mod_data_exports_server <- function(id, analysis, facilities) {
  shiny::moduleServer(id, function(input, output, session) {
    output$table <- DT::renderDT({
      value <- require_analysis_value(analysis())
      data <- export_map_table(value)
      DT::datatable(
        data,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
        class = "stripe hover compact",
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          paste("Fonte: DATASUS · consulta gerada em", format(Sys.time(), "%d/%m/%Y %H:%M"))
        )
      )
    }, server = TRUE)

    output$map_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "territorios"), ".csv"),
      content = function(file) write_csv_utf8(export_map_table(require_analysis_value(analysis())), file)
    )
    output$series_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "serie"), ".csv"),
      content = function(file) write_csv_utf8(export_series_table(require_analysis_value(analysis())), file)
    )
    output$ranking_csv <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "ranking"), ".csv"),
      content = function(file) write_csv_utf8(export_ranking_table(require_analysis_value(analysis())), file)
    )
    output$geojson <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "mapa"), ".geojson"),
      content = function(file) write_analysis_geojson(require_analysis_value(analysis()), file)
    )
    output$series_png <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "serie"), ".png"),
      content = function(file) save_plot_png(make_series_plot(require_analysis_value(analysis())), file)
    )
    output$ranking_png <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "ranking"), ".png"),
      content = function(file) save_plot_png(make_ranking_plot(require_analysis_value(analysis())), file, 11, 8)
    )
    output$map_png <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "mapa"), ".png"),
      content = function(file) save_plot_png(make_static_map_plot(require_analysis_value(analysis())), file, 12, 9)
    )
    output$map_html <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "mapa_interativo"), ".html"),
      content = function(file) save_interactive_map(build_choropleth_map(require_analysis_value(analysis())), file)
    )
    output$facilities_geojson <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "estabelecimentos"), ".geojson"),
      content = function(file) write_facilities_geojson(facilities(), file)
    )
    output$manifest <- shiny::downloadHandler(
      filename = function() paste0(safe_file_stub(require_analysis_value(analysis()), "manifesto"), ".json"),
      content = function(file) write_manifest_json(require_analysis_value(analysis()), file)
    )
  })
}
