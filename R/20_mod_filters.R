uf_choices <- function() {
  c(
    "Brasil" = "all",
    "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
    "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
    "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
    "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
    "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
    "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
    "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
    "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
    "Tocantins" = "TO"
  )
}

options_error <- function(error) {
  structure(list(message = conditionMessage(error)), class = c("datasus_options_error", "list"))
}

mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tags$div(
    class = "filter-panel",
    htmltools::tags$div(
      class = "filter-intro",
      htmltools::tags$p(class = "eyebrow", "CONSULTA ORIENTADA"),
      htmltools::tags$h2("Defina o recorte"),
      htmltools::tags$p("Os campos são definidos por adaptadores auditáveis dos arquivos públicos do DATASUS.")
    ),
    shiny::radioButtons(
      ns("interface_mode"),
      "Nível de configuração",
      choices = c("Simples" = "simple", "Avançado" = "advanced"),
      selected = "simple",
      inline = TRUE
    ),
    shiny::selectInput(ns("domain"), "Tema", choices = domain_choices(), selected = "sih_morbidade"),
    shiny::uiOutput(ns("dataset_ui")),
    shiny::selectInput(ns("uf"), "Abrangência", choices = uf_choices(), selected = "all"),
    shiny::selectInput(
      ns("geo_level"),
      "Geografia do mapa",
      choices = c(
        "Unidade da Federação" = "state",
        "Município" = "municipality",
        "Região de Saúde" = "health_region"
      ),
      selected = "municipality"
    ),
    shiny::actionButton(
      ns("load_options"),
      "Carregar opções da fonte",
      class = "btn-source",
      width = "100%"
    ),
    shiny::uiOutput(ns("source_status")),
    htmltools::tags$hr(),
    shiny::uiOutput(ns("measure_ui")),
    shiny::uiOutput(ns("period_ui")),
    shiny::uiOutput(ns("condition_field_ui")),
    shiny::conditionalPanel(
      condition = "input.condition_field && input.condition_field !== ''",
      shiny::selectizeInput(
        ns("condition_values"),
        "Valor(es) do recorte",
        choices = NULL,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          maxOptions = 100,
          loadThrottle = 250,
          create = TRUE,
          placeholder = "Digite um código ou parte da descrição"
        )
      ),
      ns = ns
    ),
    shiny::uiOutput(ns("territory_field_ui")),
    shiny::conditionalPanel(
      condition = "input.territory_field && input.territory_field !== ''",
      shiny::selectizeInput(
        ns("territory_values"),
        "Território(s)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          maxOptions = 100,
          loadThrottle = 250,
          create = TRUE,
          placeholder = "Digite o código ou nome do município"
        )
      ),
      ns = ns
    ),
    shiny::uiOutput(ns("urgency_ui")),
    shiny::uiOutput(ns("scale_ui")),
    shiny::conditionalPanel(
      condition = "input.interface_mode === 'advanced'",
      shiny::selectInput(
        ns("comparison"),
        "Referência comparativa",
        choices = c(
          "Automática (UF ou Brasil)" = "auto",
          "Sem comparação" = "none"
        ),
        selected = "auto"
      ),
      shiny::selectInput(
        ns("map_method"),
        "Classificação do mapa",
        choices = c(
          "Quantis" = "quantile", "Intervalos iguais" = "equal",
          "Logarítmica" = "log", "Limites fixos" = "fixed"
        ),
        selected = "quantile"
      ),
      shiny::conditionalPanel(
        condition = "input.map_method === 'fixed'",
        shiny::textInput(
          ns("map_fixed_breaks"),
          "Limites separados por vírgula",
          placeholder = "0, 10, 25, 50, 100"
        ),
        ns = ns
      ),
      shiny::selectInput(
        ns("top_n"), "Itens no ranking",
        choices = c("Top 10" = "10", "Top 15" = "15", "Top 25" = "25"),
        selected = "15"
      ),
      htmltools::tags$div(
        class = "cache-controls",
        shiny::uiOutput(ns("cache_status")),
        shiny::actionButton(
          ns("clear_aggregate_cache"),
          "Limpar resultados agregados",
          class = "btn-source",
          width = "100%"
        )
      ),
      ns = ns
    ),
    shiny::actionButton(
      ns("analyze"),
      "Analisar",
      class = "btn-analyze",
      width = "100%"
    ),
    htmltools::tags$p(
      class = "filter-footnote",
      "Demandas são representadas por utilização/produção registrada no SUS; não estimam necessidade reprimida."
    )
  )
}

mod_filters_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    catalog <- load_datasus_catalog()

    cache_generation <- shiny::reactiveVal(0L)
    output$cache_status <- shiny::renderUI({
      cache_generation()
      inventory <- cache_inventory()
      total <- sum(inventory$bytes, na.rm = TRUE)
      htmltools::tags$p(
        class = "source-status",
        paste0("Cache local: ", format_pt_number(total / 1024^2, 0.1), " MB")
      )
    })
    shiny::observeEvent(input$clear_aggregate_cache, {
      clear_app_cache("microdata-results")
      cache_generation(cache_generation() + 1L)
      shiny::showNotification(
        "Resultados agregados removidos; arquivos DBC verificados foram preservados.",
        type = "message"
      )
    })

    output$dataset_ui <- shiny::renderUI({
      choices <- dataset_choices(input$domain, catalog)
      default <- get_domain_config(input$domain)$default_dataset
      selected <- if (default %in% unname(choices)) default else unname(choices)[[1L]]
      shiny::selectInput(session$ns("dataset"), "Conjunto de dados", choices = choices, selected = selected)
    })

    current_signature <- shiny::reactive({
      list(
        domain = input$domain,
        dataset = input$dataset,
        uf = input$uf,
        geo_level = input$geo_level
      )
    })

    metadata <- shiny::eventReactive(input$load_options, {
      signature <- current_signature()
      if (is.null(signature$dataset) || !nzchar(signature$dataset)) {
        return(options_error(simpleError("Selecione um conjunto de dados.")))
      }
      shiny::withProgress(message = "Preparando opções da fonte...", value = 0.3, {
        result <- tryCatch(
          fetch_datasus_options(
            domain = signature$domain,
            dataset = signature$dataset,
            uf = signature$uf,
            geo_level = signature$geo_level
          ),
          error = options_error
        )
        if (!inherits(result, "datasus_options_error")) attr(result, "app_signature") <- signature
        shiny::incProgress(0.7)
        result
      })
    }, ignoreInit = TRUE)

    output$source_status <- shiny::renderUI({
      value <- metadata()
      if (inherits(value, "datasus_options_error")) {
        return(htmltools::tags$div(class = "source-status source-error", value$message))
      }
      htmltools::tags$div(
        class = "source-status source-ok",
        paste0(
          nrow(value$periodo), " períodos, ", nrow(value$conteudo),
          " medidas e ", length(value$filtros), " filtros disponíveis · ",
          value$provider_label %||% "DATASUS",
          " · disponibilidade confirmada no início da análise"
        )
      )
    })

    valid_metadata <- shiny::reactive({
      value <- metadata()
      shiny::validate(shiny::need(!inherits(value, "datasus_options_error"), value$message %||% "Fonte indisponível."))
      value
    })

    update_filter_input <- function(id, field) {
      value <- valid_metadata()
      signature <- attr(value, "app_signature")
      table <- source_filter_choices(
        value,
        domain = signature$domain,
        dataset = signature$dataset,
        field = field,
        uf = signature$uf
      )
      choices <- stats::setNames(as.character(table$value), table$id)
      shiny::updateSelectizeInput(
        session,
        id,
        choices = choices,
        selected = character(),
        server = TRUE
      )
    }

    output$measure_ui <- shiny::renderUI({
      value <- valid_metadata()
      choices <- stats::setNames(as.character(value$conteudo$value), value$conteudo$id)
      shiny::selectInput(session$ns("measure"), "Medida", choices = choices, selected = unname(choices)[[1L]])
    })

    output$period_ui <- shiny::renderUI({
      value <- valid_metadata()
      choices <- stats::setNames(as.character(value$periodo$value), value$periodo$id)
      frequency <- get_domain_config(input$domain)$frequency
      selected <- default_period_row(value$periodo, frequency)$value[[1L]]
      shiny::selectizeInput(
        session$ns("periods"),
        "Período(s)",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          maxItems = value$max_periods %||% 120,
          maxOptions = 1000
        )
      )
    })

    output$condition_field_ui <- shiny::renderUI({
      value <- valid_metadata()
      index <- classify_source_filters(value)
      index <- index[index$role %in% c("condition", "other"), , drop = FALSE]
      choices <- c("Sem filtro adicional" = "")
      if (nrow(index) > 0L) choices <- c(choices, stats::setNames(index$field, index$label))
      shiny::selectInput(
        session$ns("condition_field"),
        "Condição, procedimento ou outro recorte",
        choices = choices,
        selected = ""
      )
    })

    shiny::observeEvent(list(metadata(), input$condition_field), {
      field <- input$condition_field
      if (is.null(field) || !nzchar(field)) return()
      update_filter_input("condition_values", field)
    }, ignoreInit = TRUE)

    output$territory_field_ui <- shiny::renderUI({
      value <- valid_metadata()
      index <- classify_source_filters(value)
      index <- index[index$role == "territory", , drop = FALSE]
      choices <- c("Sem restrição territorial adicional" = "")
      if (nrow(index) > 0L) choices <- c(choices, stats::setNames(index$field, index$label))
      shiny::selectInput(
        session$ns("territory_field"),
        "Filtro territorial opcional",
        choices = choices,
        selected = ""
      )
    })

    shiny::observeEvent(list(metadata(), input$territory_field), {
      field <- input$territory_field
      if (is.null(field) || !nzchar(field)) return()
      update_filter_input("territory_values", field)
    }, ignoreInit = TRUE)

    output$urgency_ui <- shiny::renderUI({
      value <- valid_metadata()
      if (is.null(resolve_urgency_filter(value))) return(NULL)
      shiny::checkboxInput(
        session$ns("urgent_only"),
        "Somente atendimentos de urgência",
        FALSE
      )
    })

    output$scale_ui <- shiny::renderUI({
      value <- valid_metadata()
      measure <- value$conteudo[value$conteudo$value %in% input$measure, , drop = FALSE]
      choices <- c("Valor observado" = "count")
      if (nrow(measure) == 1L && measure_rate_eligible(measure)) {
        multiplier <- format_pt_number(measure$multiplier[[1L]], accuracy = 1)
        choices <- c(choices, stats::setNames("rate", paste("Taxa bruta por", multiplier)))
      }
      if (nrow(measure) == 1L && isTRUE(measure$standardizable[[1L]])) {
        choices <- c(
          choices,
          "Taxa padronizada por idade (denominadores 2010)" = "age_standardized_rate"
        )
      }
      shiny::radioButtons(
        session$ns("scale"),
        "Métrica exibida",
        choices = choices,
        selected = "count",
        inline = TRUE
      )
    })

    query <- shiny::reactive({
      value <- valid_metadata()
      signature <- attr(value, "app_signature")
      shiny::validate(shiny::need(
        identical(signature, current_signature()),
        "Os campos de fonte ou geografia mudaram. Carregue novamente as opções da fonte."
      ))
      shiny::validate(shiny::need(length(input$periods) > 0L, "Selecione ao menos um período."))

      measure <- value$conteudo[value$conteudo$value %in% input$measure, , drop = FALSE]
      periods <- value$periodo[value$periodo$value %in% input$periods, , drop = FALSE]
      shiny::validate(shiny::need(nrow(measure) == 1L, "Selecione uma medida válida."))
      shiny::validate(shiny::need(nrow(periods) > 0L, "Selecione períodos válidos."))
      fixed_breaks <- suppressWarnings(as.numeric(trimws(strsplit(
        input$map_fixed_breaks %||% "", ",", fixed = TRUE
      )[[1L]])))
      fixed_breaks <- fixed_breaks[is.finite(fixed_breaks)]

      new_datasus_query(
        domain = input$domain,
        dataset = input$dataset,
        uf = input$uf,
        geo_level = input$geo_level,
        measure = measure,
        periods = periods,
        options = value,
        condition_field = input$condition_field,
        condition_values = input$condition_values %||% character(),
        territory_field = input$territory_field,
        territory_values = input$territory_values %||% character(),
        urgent_only = isTRUE(input$urgent_only),
        scale = input$scale %||% "count",
        comparison = input$comparison %||% "auto",
        map_method = input$map_method %||% "quantile",
        map_fixed_breaks = fixed_breaks,
        top_n = input$top_n %||% "15",
        interface_mode = input$interface_mode %||% "simple"
      )
    })

    list(
      query = query,
      metadata = metadata,
      analyze_trigger = shiny::reactive(input$analyze)
    )
  })
}
