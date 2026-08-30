order_series_data <- function(series, query) {
  order_value <- vapply(
    series$label,
    period_to_year_month,
    integer(1),
    period_value = ""
  )
  fallback <- seq_len(nrow(series))
  order_value[is.na(order_value)] <- max(c(order_value, 0), na.rm = TRUE) + fallback[is.na(order_value)]
  series[order(order_value), , drop = FALSE]
}

select_display_metric <- function(bundle, query) {
  rate_available <- query$scale == "rate" && "rate" %in% names(bundle$map) &&
    any(!is.na(bundle$map$rate))
  if (query$scale == "rate" && !rate_available) {
    bundle$warnings <- unique(c(
      bundle$warnings,
      "As taxas solicitadas ficaram indisponíveis; os resultados exibem totais."
    ))
  }

  metric <- if (rate_available) "rate" else "value"
  metric_label <- if (rate_available) {
    if (query$frequency == "monthly") "Taxa por 100 mil pessoas-ano" else "Taxa por 100 mil habitantes"
  } else {
    query$measure_label
  }

  for (name in c("map", "ranking", "series")) {
    data <- bundle[[name]]
    data$display_value <- data[[metric]]
    bundle[[name]] <- data
  }
  bundle$metric <- metric
  bundle$metric_label <- metric_label
  bundle
}

summarize_analysis <- function(bundle, query) {
  map <- bundle$map
  valid <- !is.na(map$display_value)
  total_events <- sum(map$value, na.rm = TRUE)
  total_denominator <- if ("denominator" %in% names(map)) sum(map$denominator, na.rm = TRUE) else NA_real_
  overall_rate <- if (bundle$metric == "rate") {
    calculate_crude_rate(total_events, total_denominator)
  } else {
    NA_real_
  }

  top_index <- if (any(valid)) which.max(map$display_value) else NA_integer_
  list(
    total = total_events,
    overall_rate = overall_rate,
    top_name = if (!is.na(top_index)) map$geo_name[[top_index]] else "Sem dados",
    top_value = if (!is.na(top_index)) map$display_value[[top_index]] else NA_real_,
    territories_with_data = sum(valid),
    periods = nrow(query$periods),
    latest_period = query$periods$id[[nrow(query$periods)]],
    metric = bundle$metric,
    metric_label = bundle$metric_label
  )
}

package_versions_manifest <- function() {
  packages <- c(
    "R", "shiny", "datasus", "datasusr", "microdatasus", "geobr",
    "sidrar", "sf", "dplyr", "ggplot2", "leaflet"
  )
  versions <- vapply(packages, function(package) {
    if (package == "R") return(as.character(getRversion()))
    safe_package_version(package)
  }, character(1))
  as.list(versions)
}

sanitize_provenance <- function(provenance) {
  lapply(provenance, function(item) {
    if (!is.list(item)) return(item)
    lapply(item, function(value) {
      if (inherits(value, "POSIXt")) format(value, "%Y-%m-%dT%H:%M:%S%z") else value
    })
  })
}

build_analysis_manifest <- function(query, bundle, geometry) {
  list(
    application = list(name = APP_NAME, version = APP_VERSION),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    query = query_to_manifest(query),
    result = list(
      metric = bundle$metric,
      metric_label = bundle$metric_label,
      ranking_kind = bundle$ranking_kind,
      geometry_year = geometry$geometry_year %||% NA_integer_,
      retrieved_at = format(bundle$retrieved_at, "%Y-%m-%dT%H:%M:%S%z")
    ),
    source_provenance = sanitize_provenance(bundle$provenance),
    package_versions = package_versions_manifest(),
    warnings = bundle$warnings,
    interpretation = c(
      "SIH registra autorizações/internações, não pacientes únicos.",
      "SIA registra produção aprovada, não pessoas atendidas únicas.",
      "CNES representa estoque cadastral por competência e não demanda não atendida.",
      "Taxas dependem da disponibilidade dos denominadores oficiais."
    )
  )
}

run_analysis <- function(query, refresh = FALSE) {
  validate_datasus_query(query)
  bundle <- run_tabnet_bundle(query, refresh)
  bundle$map <- normalize_geographic_data(bundle$map, query$geo_level)

  if (query$scale == "rate") {
    bundle <- enrich_bundle_with_rates(bundle, query, refresh)
  }
  bundle <- select_display_metric(bundle, query)
  bundle$series <- order_series_data(bundle$series, query)
  bundle$ranking <- dplyr::arrange(bundle$ranking, dplyr::desc(.data$display_value))
  bundle$ranking <- utils::head(bundle$ranking, 20L)

  geometry <- tryCatch(
    join_analysis_geometry(bundle$map, query, bundle$map_periods, refresh),
    error = function(error) {
      bundle$warnings <<- unique(c(
        bundle$warnings,
        paste("Mapa territorial indisponível:", conditionMessage(error))
      ))
      list(data = NULL, geometry_year = NA_integer_, unmatched = NULL)
    }
  )
  if (!is.null(geometry$unmatched) && nrow(geometry$unmatched) > 0L) {
    bundle$warnings <- unique(c(
      bundle$warnings,
      paste0(nrow(geometry$unmatched), " linha(s) do TABNET não encontraram geometria correspondente.")
    ))
  }

  summary <- summarize_analysis(bundle, query)
  manifest <- build_analysis_manifest(query, bundle, geometry)
  result <- list(
    query = query,
    summary = summary,
    map_data = bundle$map,
    map_sf = geometry$data,
    geometry_year = geometry$geometry_year,
    series = bundle$series,
    ranking = bundle$ranking,
    ranking_kind = bundle$ranking_kind,
    metric = bundle$metric,
    metric_label = bundle$metric_label,
    population = bundle$population %||% NULL,
    provenance = bundle$provenance,
    warnings = bundle$warnings,
    manifest = manifest
  )
  class(result) <- c("datasus_analysis", "list")
  result
}
