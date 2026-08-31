APP_NAME <- "Painel Nacional de Dados do SUS"
APP_VERSION <- "0.1.1"

REQUIRED_PACKAGES <- c(
  "bslib", "cachem", "datasus", "datasusr", "digest", "dplyr", "DT",
  "geobr", "ggplot2", "htmltools", "htmlwidgets", "jsonlite", "leaflet",
  "microdatasus", "purrr", "scales", "sf", "shiny", "sidrar", "stringi",
  "stringr", "tibble", "tidyr"
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
    unset = file.path("data", "cache")
  )
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

export_directory <- function() {
  normalizePath("exports", winslash = "/", mustWork = FALSE)
}

check_runtime_dependencies <- function(packages = REQUIRED_PACKAGES) {
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}
