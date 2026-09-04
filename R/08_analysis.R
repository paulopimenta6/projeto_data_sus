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
  standardized_available <- query$scale == "age_standardized_rate" &&
    "age_standardized_rate" %in% names(bundle$map) &&
    any(!is.na(bundle$map$age_standardized_rate))
  rate_available <- query$scale %in% c("rate", "age_standardized_rate") &&
    "rate" %in% names(bundle$map) &&
    any(!is.na(bundle$map$rate))
  if (query$scale == "age_standardized_rate" && !standardized_available) {
    bundle$warnings <- unique(c(
      bundle$warnings,
      "A taxa padronizada solicitada ficou indisponível; o resultado exibe a taxa bruta quando possível."
    ))
  }
  if (query$scale %in% c("rate", "age_standardized_rate") && !rate_available) {
    bundle$warnings <- unique(c(
      bundle$warnings,
      "As taxas solicitadas ficaram indisponíveis; os resultados exibem totais."
    ))
  }

  metric <- if (standardized_available) {
    "age_standardized_rate"
  } else if (rate_available) {
    "rate"
  } else {
    "value"
  }
  unit_multiplier <- format_pt_number(query$measure_spec$multiplier, accuracy = 1)
  metric_label <- if (metric == "age_standardized_rate") {
    paste("Taxa padronizada por idade por", unit_multiplier)
  } else if (metric == "rate") {
    if (query$frequency == "monthly") {
      paste("Taxa bruta por", unit_multiplier, "pessoas-ano")
    } else {
      paste("Taxa bruta por", unit_multiplier, "habitantes")
    }
  } else if (query$measure_spec$measure_type == "proportion") {
    paste0(query$measure_label, " (%)")
  } else {
    paste(query$measure_label, "(", query$measure_spec$unit, ")")
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
  total_events <- bundle$total %||% sum_or_na(map$value)
  observed_events <- !is.na(map$value)
  complete_denominator <- "denominator" %in% names(map) && any(observed_events) &&
    all(!is.na(map$denominator[observed_events]))
  total_denominator <- if (complete_denominator) {
    sum_or_na(map$denominator[observed_events])
  } else {
    NA_real_
  }
  overall_rate <- if (bundle$metric == "rate") {
    calculate_crude_rate(
      sum_or_na(map$value[observed_events]), total_denominator,
      query$measure_spec$multiplier
    )
  } else if (bundle$metric == "age_standardized_rate") {
    bundle$overall_standardized$rate %||% NA_real_
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
    territories_zero = sum(valid & map$display_value == 0),
    territories_missing = sum(!valid),
    periods = nrow(query$periods),
    latest_period = latest_period_row(query$periods)$id[[1L]],
    metric = bundle$metric,
    metric_label = bundle$metric_label
  )
}

build_quality_summary <- function(bundle, geometry) {
  map <- bundle$map
  observed <- !is.na(map$display_value)
  denominator_missing <- if ("denominator" %in% names(map)) {
    sum(!is.na(map$value) & is.na(map$denominator))
  } else {
    0L
  }
  tibble::tibble(
    indicator = c(
      "territorios_validos", "territorios_zero", "territorios_sem_dado",
      "territorios_sem_geometria", "denominadores_ausentes",
      "registros_sem_medida", "registros_sem_idade"
    ),
    value = c(
      sum(observed), sum(observed & map$display_value == 0), sum(!observed),
      nrow(geometry$unmatched %||% data.frame()), denominator_missing,
      bundle$quality$missing_measure_records %||% 0,
      bundle$quality$missing_age_records %||% 0
    )
  )
}

build_comparisons <- function(bundle, query) {
  map <- bundle$map
  valid <- !is.na(map$display_value)
  reference_label <- if (is.null(query$uf)) "Brasil" else paste("UF", query$uf)
  reference_scalar <- if (bundle$metric == "rate") {
    observed <- !is.na(map$value)
    if (!any(observed) || any(is.na(map$denominator[observed]))) {
      NA_real_
    } else {
      calculate_crude_rate(
        sum_or_na(map$value[observed]),
        sum_or_na(map$denominator[observed]),
        query$measure_spec$multiplier
      )
    }
  } else if (bundle$metric == "age_standardized_rate") {
    bundle$overall_standardized$rate %||% NA_real_
  } else {
    sum_or_na(map$value)
  }
  tibble::tibble(
    geo_code = map$geo_code[valid],
    geo_name = map$geo_name[valid],
    value = map$display_value[valid],
    reference = reference_label,
    reference_value = reference_scalar,
    difference = map$display_value[valid] - reference_scalar,
    ratio = if (is.finite(reference_scalar) && reference_scalar != 0) {
      map$display_value[valid] / reference_scalar
    } else {
      rep(NA_real_, sum(valid))
    }
  )
}

build_auditable_insights <- function(bundle, query, comparisons, quality) {
  insights <- list()
  series <- bundle$series[!is.na(bundle$series$display_value), , drop = FALSE]
  if (nrow(series) >= 2L) {
    current <- series[nrow(series), , drop = FALSE]
    previous <- series[nrow(series) - 1L, , drop = FALSE]
    difference <- current$display_value - previous$display_value
    percent <- if (previous$display_value != 0) difference / previous$display_value * 100 else NA_real_
    insights[[length(insights) + 1L]] <- list(
      id = "period_change",
      title = "Mudança no período mais recente",
      text = paste0(
        current$label, " ficou ", format_pt_number(abs(difference), 0.1),
        if (difference >= 0) " acima" else " abaixo", " de ", previous$label,
        if (is.na(percent)) "." else paste0(" (", format_pt_number(percent, 0.1), "%).")
      ),
      formula = "valor_atual - valor_anterior; diferença / valor_anterior × 100",
      caveat = "Comparação descritiva; não demonstra causalidade."
    )
  }
  if (nrow(comparisons) > 0L && any(is.finite(comparisons$ratio))) {
    top <- comparisons[which.max(comparisons$ratio), , drop = FALSE]
    insights[[length(insights) + 1L]] <- list(
      id = "reference_comparison",
      title = "Maior razão frente à referência",
      text = paste0(
        top$geo_name, ": ", format_pt_number(top$ratio, 0.01), " vez(es) o valor de ",
        top$reference, "."
      ),
      formula = "valor_territorial / valor_referência",
      caveat = "A comparação usa os mesmos filtros e unidade."
    )
  }
  if (
    query$measure_spec$measure_type %in% c("count", "amount") &&
      sum(bundle$map$value, na.rm = TRUE) > 0
  ) {
    share <- max(bundle$map$value, na.rm = TRUE) / sum(bundle$map$value, na.rm = TRUE) * 100
    insights[[length(insights) + 1L]] <- list(
      id = "concentration",
      title = "Concentração territorial",
      text = paste0("O território líder concentra ", format_pt_number(share, 0.1), "% do total."),
      formula = "maior_total_territorial / total_do_recorte × 100",
      caveat = "Concentração pode refletir tamanho populacional e fluxo assistencial."
    )
  }
  insights
}

package_versions_manifest <- function() {
  packages <- c(
    "R", "shiny", "datasusr", "microdatasus", "geobr",
    "sidrar", "sf", "dplyr", "ggplot2", "leaflet", "knitr", "rmarkdown"
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
    schema_version = ANALYSIS_MANIFEST_VERSION,
    application = list(name = APP_NAME, version = APP_VERSION),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    query = query_to_manifest(query),
    result = list(
      metric = bundle$metric,
      metric_label = bundle$metric_label,
      ranking_kind = bundle$ranking_kind,
      geometry_year = geometry$geometry_year %||% NA_integer_,
      map_classification = bundle$classification %||% NULL,
      retrieved_at = format(bundle$retrieved_at, "%Y-%m-%dT%H:%M:%S%z")
    ),
    source_provenance = sanitize_provenance(bundle$provenance),
    quality = bundle$quality_summary %||% NULL,
    insights = bundle$insights %||% list(),
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
  bundle <- run_datasus_bundle(query, refresh)
  bundle$map <- normalize_geographic_data(bundle$map, query$geo_level)

  if (query$scale %in% c("rate", "age_standardized_rate")) {
    bundle <- enrich_bundle_with_rates(bundle, query, refresh)
  }
  if (query$scale == "age_standardized_rate") {
    bundle <- enrich_standardized_rates(bundle, query, refresh)
  }
  bundle <- select_display_metric(bundle, query)
  bundle$series <- order_series_data(bundle$series, query)
  bundle$ranking <- dplyr::arrange(bundle$ranking, dplyr::desc(.data$display_value))

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
      paste0(nrow(geometry$unmatched), " território(s) da fonte não encontraram geometria correspondente.")
    ))
  }

  bundle$classification <- classify_map_values(
    bundle$map$display_value,
    method = query$map_method,
    fixed_breaks = query$map_fixed_breaks
  )
  bundle$quality_summary <- build_quality_summary(bundle, geometry)
  comparisons <- if (query$comparison == "none") {
    tibble::tibble()
  } else {
    build_comparisons(bundle, query)
  }
  bundle$insights <- build_auditable_insights(
    bundle, query, comparisons, bundle$quality_summary
  )
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
    measure_spec = query$measure_spec,
    geography_semantics = microdata_source_spec(query$domain, query$dataset)$geography_semantics,
    classification = bundle$classification,
    comparisons = comparisons,
    insights = bundle$insights,
    quality = bundle$quality_summary,
    population = bundle$population %||% NULL,
    population_age = bundle$population_age %||% NULL,
    provenance = bundle$provenance,
    warnings = bundle$warnings,
    manifest = manifest
  )
  class(result) <- c("datasus_analysis", "list")
  result
}
