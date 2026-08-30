missing_packages <- check_runtime_dependencies()

if (length(missing_packages) > 0L) {
  stop(
    paste0(
      "Dependências ausentes: ",
      paste(missing_packages, collapse = ", "),
      ". Execute `Rscript scripts/setup.R` na raiz do projeto."
    ),
    call. = FALSE
  )
}

options(
  datasus.timeout = 60,
  datasus.download_timeout = 600,
  datasus.max_tries = 2,
  shiny.maxRequestSize = 100 * 1024^2
)

shiny::shinyApp(ui = app_ui(), server = app_server)
