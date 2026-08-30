mod_maps_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    id = ns("map_tabs"),
    bslib::nav_panel(
      "Mapa coroplético",
      htmltools::tags$div(
        class = "map-toolbar",
        htmltools::tags$p("Clique em um território para consultar o valor e a métrica aplicada."),
        shiny::uiOutput(ns("map_note"))
      ),
      leaflet::leafletOutput(ns("choropleth"), height = "680px")
    ),
    bslib::nav_panel(
      "Estabelecimentos CNES",
      htmltools::tags$div(
        class = "facility-controls",
        shiny::selectInput(
          ns("facility_type"),
          "Tipo de estabelecimento",
          choices = facility_type_choices(),
          selected = "hospital"
        ),
        shiny::actionButton(ns("load_facilities"), "Carregar pontos", class = "btn-source"),
        shiny::uiOutput(ns("facility_status"))
      ),
      leaflet::leafletOutput(ns("facility_map"), height = "620px")
    )
  )
}

mod_maps_server <- function(id, analysis) {
  shiny::moduleServer(id, function(input, output, session) {
    facilities <- shiny::reactiveVal(NULL)
    facility_failure <- shiny::reactiveVal(NULL)

    shiny::observeEvent(analysis(), {
      facilities(NULL)
      facility_failure(NULL)
    }, ignoreInit = TRUE)

    output$choropleth <- leaflet::renderLeaflet({
      value <- require_analysis_value(analysis())
      shiny::validate(shiny::need(!is.null(value$map_sf), "A geometria territorial está indisponível."))
      build_choropleth_map(value)
    })

    output$map_note <- shiny::renderUI({
      value <- require_analysis_value(analysis())
      htmltools::tags$span(
        class = "source-chip",
        paste("Limites territoriais:", value$geometry_year)
      )
    })

    shiny::observeEvent(input$load_facilities, {
      value <- analysis()
      if (is.null(value) || inherits(value, "datasus_analysis_error")) {
        facility_failure("Execute uma análise válida antes de carregar estabelecimentos.")
        return()
      }
      facility_failure(NULL)
      result <- shiny::withProgress(
        message = "Carregando estabelecimentos georreferenciados...",
        value = 0.2,
        {
          loaded <- tryCatch(
            load_health_facilities(value$query, input$facility_type),
            error = function(error) error
          )
          shiny::incProgress(0.8)
          loaded
        }
      )
      if (inherits(result, "error")) {
        facilities(NULL)
        facility_failure(conditionMessage(result))
      } else {
        facilities(result)
      }
    })

    output$facility_status <- shiny::renderUI({
      failure <- facility_failure()
      if (!is.null(failure)) {
        return(htmltools::tags$span(class = "source-status source-error", failure))
      }
      value <- facilities()
      if (is.null(value)) {
        return(htmltools::tags$span(
          class = "source-status",
          "Selecione uma UF na consulta principal e carregue os pontos sob demanda."
        ))
      }
      htmltools::tags$span(
        class = "source-status source-ok",
        paste0(format_pt_number(nrow(value)), " estabelecimentos · competência ", unique(value$facility_date)[[1L]])
      )
    })

    output$facility_map <- leaflet::renderLeaflet({
      value <- facilities()
      shiny::validate(shiny::need(!is.null(value), "Os pontos ainda não foram carregados."))
      build_facility_map(value)
    })

    facilities
  })
}
