APP_NAME <- "Painel Nacional de Dados do SUS"
APP_VERSION <- "0.3.0"
CACHE_SCHEMA_VERSION <- 3L
ANALYSIS_MANIFEST_VERSION <- 2L
PROJECT_ROOT <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

project_path <- function(...) {
  file.path(PROJECT_ROOT, ...)
}

REQUIRED_PACKAGES <- c(
  "bslib", "cachem", "datasusr", "digest", "dplyr", "DT",
  "geobr", "ggplot2", "htmltools", "htmlwidgets", "jsonlite", "leaflet",
  "knitr", "microdatasus", "purrr", "rmarkdown", "scales", "sf", "shiny",
  "sidrar", "stringi", "stringr", "tibble", "tidyr"
)

APP_COLORS <- list(
  navy = "#0B2D3A",
  teal = "#087E8B",
  cyan = "#22A6B3",
  gold = "#F4B942",
  coral = "#E76F51",
  ink = "#17313B",
  mist = "#EEF5F4"
)

cache_directory <- function() {
  path <- Sys.getenv(
    "PROJETO_DATASUS_CACHE_DIR",
    unset = project_path("data", "cache")
  )
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

export_directory <- function() {
  normalizePath(project_path("exports"), winslash = "/", mustWork = FALSE)
}

check_runtime_dependencies <- function(packages = REQUIRED_PACKAGES) {
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}
