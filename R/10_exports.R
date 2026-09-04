safe_file_stub <- function(analysis, suffix = NULL) {
  pieces <- c(
    normalize_text(analysis$query$domain),
    normalize_text(analysis$query$dataset),
    format(Sys.Date(), "%Y%m%d"),
    suffix
  )
  paste(pieces[nzchar(pieces)], collapse = "_")
}

export_map_table <- function(analysis) {
  columns <- intersect(
    c(
      "geo_code", "geo_name", "value", "eligible", "missing", "denominator",
      "rate", "rate_low", "rate_high", "age_standardized_rate",
      "age_standardized_low", "age_standardized_high"
    ),
    names(analysis$map_data)
  )
  result <- analysis$map_data[, columns, drop = FALSE]
  names(result) <- c(
    geo_code = "codigo_territorio",
    geo_name = "territorio",
    value = "total",
    eligible = "registros_elegiveis",
    missing = "registros_ignorados",
    denominator = "denominador_pessoas_ano",
    rate = "taxa_100_mil",
    rate_low = "taxa_ic95_inferior",
    rate_high = "taxa_ic95_superior",
    age_standardized_rate = "taxa_padronizada_idade",
    age_standardized_low = "taxa_padronizada_ic95_inferior",
    age_standardized_high = "taxa_padronizada_ic95_superior"
  )[names(result)]
  result
}

export_series_table <- function(analysis) {
  columns <- intersect(
    c(
      "label", "year", "value", "eligible", "missing", "denominator", "rate",
      "rate_low", "rate_high", "age_standardized_rate", "age_standardized_low",
      "age_standardized_high"
    ),
    names(analysis$series)
  )
  result <- analysis$series[, columns, drop = FALSE]
  names(result) <- c(
    label = "periodo",
    year = "ano",
    value = "total",
    eligible = "registros_elegiveis",
    missing = "registros_ignorados",
    denominator = "populacao",
    rate = "taxa_100_mil",
    rate_low = "taxa_ic95_inferior",
    rate_high = "taxa_ic95_superior",
    age_standardized_rate = "taxa_padronizada_idade",
    age_standardized_low = "taxa_padronizada_ic95_inferior",
    age_standardized_high = "taxa_padronizada_ic95_superior"
  )[names(result)]
  result
}

export_ranking_table <- function(analysis) {
  columns <- intersect(
    c(
      "label", "value", "eligible", "missing", "rate", "rate_low", "rate_high",
      "age_standardized_rate", "age_standardized_low", "age_standardized_high"
    ),
    names(analysis$ranking)
  )
  result <- analysis$ranking[, columns, drop = FALSE]
  names(result) <- c(
    label = "categoria",
    value = "total",
    eligible = "registros_elegiveis",
    missing = "registros_ignorados",
    rate = "taxa_100_mil",
    rate_low = "taxa_ic95_inferior",
    rate_high = "taxa_ic95_superior",
    age_standardized_rate = "taxa_padronizada_idade",
    age_standardized_low = "taxa_padronizada_ic95_inferior",
    age_standardized_high = "taxa_padronizada_ic95_superior"
  )[names(result)]
  result
}

write_csv_utf8 <- function(data, path) {
  utils::write.csv(data, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  invisible(path)
}

write_analysis_geojson <- function(analysis, path) {
  if (is.null(analysis$map_sf)) stop("O mapa territorial está indisponível.", call. = FALSE)
  data <- sf::st_transform(analysis$map_sf, 4326)
  sf::st_write(data, path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
  invisible(path)
}

write_facilities_geojson <- function(facilities, path) {
  if (is.null(facilities)) stop("Carregue o mapa de estabelecimentos primeiro.", call. = FALSE)
  geometry_column <- attr(facilities, "sf_column") %||% "geometry"
  export_columns <- intersect(
    c(
      "co_cnes", "facility_name", "name_muni", "code_muni",
      "facility_type_code", "facility_date", "coordinate_source",
      "coordinate_precision", "coordinate_quality", geometry_column
    ),
    names(facilities)
  )
  facilities <- facilities[, export_columns, drop = FALSE]
  sf::st_write(
    sf::st_transform(facilities, 4326),
    path,
    driver = "GeoJSON",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  invisible(path)
}

write_manifest_json <- function(analysis, path) {
  jsonlite::write_json(
    analysis$manifest,
    path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  invisible(path)
}

save_plot_png <- function(plot, path, width = 11, height = 7) {
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300, bg = "white")
  invisible(path)
}

write_analysis_report <- function(analysis, path) {
  template <- project_path("templates", "analysis_report.Rmd")
  if (!file.exists(template)) stop("O modelo do relatório HTML não foi encontrado.", call. = FALSE)
  output <- rmarkdown::render(
    input = template,
    output_file = basename(path),
    output_dir = dirname(path),
    params = list(analysis = analysis),
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    clean = TRUE
  )
  if (!file.exists(output)) stop("O relatório HTML não foi gerado.", call. = FALSE)
  invisible(path)
}

save_interactive_map <- function(widget, path) {
  htmlwidgets::saveWidget(widget, path, selfcontained = TRUE, title = APP_NAME)
  invisible(path)
}
