app_ui <- function() {
  bslib::page_fillable(
    theme = bslib::bs_theme(
      version = 5,
      primary = APP_COLORS$teal,
      secondary = APP_COLORS$gold,
      bg = "#F7FAF9",
      fg = APP_COLORS$ink,
      base_font = bslib::font_google("Source Sans 3"),
      heading_font = bslib::font_google("DM Sans")
    ),
    lang = "pt-BR",
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "styles.css")
    ),
    htmltools::tags$main(
      class = "splash-shell",
      htmltools::tags$section(
        class = "splash-card",
        htmltools::tags$p(class = "eyebrow", "DATASUS · ANÁLISE TERRITORIAL"),
        htmltools::tags$h1(APP_NAME),
        htmltools::tags$p(
          class = "splash-copy",
          "Estrutura inicial pronta. Os módulos de consulta, indicadores e mapas serão carregados nesta interface."
        ),
        bslib::badge("Versão 0.1.0", bg = APP_COLORS$teal)
      )
    )
  )
}
