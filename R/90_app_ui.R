app_ui <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    primary = APP_COLORS$teal,
    secondary = APP_COLORS$gold,
    bg = "#F7FAF9",
    fg = APP_COLORS$ink,
    base_font = bslib::font_google("Source Sans 3"),
    heading_font = bslib::font_google("DM Sans")
  )

  bslib::page_sidebar(
    title = htmltools::tags$div(
      class = "app-brand",
      htmltools::tags$span(class = "brand-mark", "SUS"),
      htmltools::tags$span(
        htmltools::tags$strong("Atlas de ocorrências e serviços"),
        htmltools::tags$small("Dados públicos · análise local")
      )
    ),
    sidebar = bslib::sidebar(
      mod_filters_ui("filters"),
      width = "390px",
      open = "desktop"
    ),
    theme = theme,
    fillable = TRUE,
    lang = "pt-BR",
    htmltools::tags$head(
      htmltools::tags$link(rel = "icon", href = "favicon.svg", type = "image/svg+xml"),
      htmltools::tags$link(rel = "stylesheet", href = "styles.css"),
      htmltools::tags$meta(
        name = "description",
        content = "Painel local para análise territorial de dados públicos do SUS"
      )
    ),
    shiny::uiOutput("app_status"),
    bslib::navset_card_tab(
      id = "main_navigation",
      bslib::nav_panel(
        "Visão geral",
        htmltools::tags$div(
          class = "dashboard-section",
          mod_summary_ui("summary"),
          mod_charts_ui("charts")
        )
      ),
      bslib::nav_panel("Mapas", mod_maps_ui("maps")),
      bslib::nav_panel("Dados e exportação", mod_data_exports_ui("data_exports")),
      bslib::nav_panel(
        "Metodologia",
        htmltools::tags$article(
          class = "method-card",
          htmltools::tags$p(class = "eyebrow", "COMO INTERPRETAR"),
          htmltools::tags$h2("Escopo, fontes e limitações"),
          htmltools::tags$p(
            paste(
              "O painel baixa apenas as colunas necessárias dos arquivos DBC públicos, aplica os",
              "filtros localmente e produz tabelas agregadas para gráficos, mapas e exportações."
            )
          ),
          htmltools::tags$h3("O que significa demanda"),
          htmltools::tags$p(
            paste(
              "Demanda observada significa produção ou utilização registrada: AIH no SIH,",
              "procedimentos aprovados no SIA e notificações nos sistemas epidemiológicos.",
              "Não mede filas, necessidade não atendida nem pessoas únicas."
            )
          ),
          htmltools::tags$h3("Taxas"),
          htmltools::tags$p(
            paste(
              "Taxas brutas utilizam população oficial municipal. Agregações mensais são",
              "expressas por 100 mil pessoas-ano; cada ponto mensal usa um doze avos da",
              "população anual. Anos sem denominador municipal oficial compatível ficam sem taxa."
            )
          ),
          htmltools::tags$h3("Fontes principais"),
          htmltools::tags$ul(
            htmltools::tags$li("SIM: mortalidade por causa básica e município de residência."),
            htmltools::tags$li("SIH: internações financiadas pelo SUS e morbidade hospitalar."),
            htmltools::tags$li("SIA: produção ambulatorial aprovada."),
            htmltools::tags$li("SINAN: dengue, chikungunya, zika, malária e leptospirose."),
            htmltools::tags$li("CNES: estabelecimentos e capacidade cadastrada por competência.")
          ),
          htmltools::tags$p(
            htmltools::tags$a(
              href = "https://datasus.saude.gov.br/",
              target = "_blank",
              rel = "noopener",
              "Portal DATASUS"
            ),
            " · ",
            htmltools::tags$a(
              href = "https://rfsaldanha.github.io/microdatasus/",
              target = "_blank",
              rel = "noopener",
              "Documentação microdatasus"
            )
          )
        )
      )
    ),
    htmltools::tags$footer(
      class = "app-footer",
      paste0(APP_NAME, " v", APP_VERSION, " · resultados públicos e agregados")
    )
  )
}
