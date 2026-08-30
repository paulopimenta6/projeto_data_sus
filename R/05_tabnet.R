fetch_datasus_options <- function(domain, dataset, uf = NULL, geo_level = "state", refresh = FALSE) {
  config <- get_domain_config(domain)
  if (identical(uf, "all") || identical(uf, "")) uf <- NULL

  key <- list(
    package_version = safe_package_version("datasus"),
    domain = domain,
    dataset = dataset,
    uf = uf,
    geo_level = geo_level
  )

  cached_call(
    namespace = "tabnet-options",
    key = key,
    max_age = 6 * 60 * 60,
    refresh = refresh,
    function_to_run = function() {
      arguments <- list(
        sistema = config$system,
        conjunto = dataset,
        uf = uf
      )
      if (domain == "sim") {
        arguments$abrangencia <- if (geo_level == "state") "uf" else "municipio"
        if (geo_level == "state") arguments$uf <- NULL
      }
      do.call(datasus::datasus_opcoes, arguments)
    }
  )
}

add_urgency_filter <- function(filters, options) {
  filter_index <- classify_source_filters(options)
  urgency <- filter_index[filter_index$role == "urgency", , drop = FALSE]
  if (nrow(urgency) == 0L) {
    return(list(
      filters = filters,
      warnings = "A fonte selecionada não oferece um filtro de urgência reconhecível."
    ))
  }

  field <- urgency$field[[1L]]
  choices <- options$filtros[[field]]
  selected <- resolve_option(choices, c("Urgência", "Urgente", "Emergência"))
  if (is.null(selected)) {
    return(list(
      filters = filters,
      warnings = "O formulário TABNET não apresentou uma opção explícita de urgência."
    ))
  }

  filters[[field]] <- unique(c(filters[[field]] %||% character(), selected$value[[1L]]))
  list(filters = filters, warnings = character())
}

build_source_arguments <- function(query, line_value, periods = query$periods) {
  options <- query$options
  inactive <- resolve_inactive_column(options)
  filters <- query$filters
  warnings <- character()

  if (isTRUE(query$urgent_only)) {
    urgency <- add_urgency_filter(filters, options)
    filters <- urgency$filters
    warnings <- c(warnings, urgency$warnings)
  }

  common <- list(
    uf = query$uf,
    linha = line_value,
    conteudo = query$measure_value,
    periodo = as.character(periods$value),
    filtros = filters
  )
  if (!is.null(inactive)) common$coluna <- inactive$value[[1L]]

  arguments <- switch(
    query$domain,
    sim = c(
      list(
        conjunto = query$dataset,
        abrangencia = if (query$geo_level == "state") "uf" else "municipio"
      ),
      common
    ),
    sinan = c(list(agravo = query$dataset), common),
    c(list(conjunto = query$dataset), common)
  )
  if (query$domain == "sim" && query$geo_level == "state") arguments$uf <- NULL

  list(arguments = arguments, warnings = warnings)
}

execute_tabnet_query <- function(
  query,
  line_option,
  periods = query$periods,
  purpose = "analysis",
  refresh = FALSE
) {
  source <- build_source_arguments(query, line_option$value[[1L]], periods)
  cache_key <- list(
    package_version = safe_package_version("datasus"),
    source_function = query$source_function,
    arguments = source$arguments
  )

  cached_call(
    namespace = "tabnet-results",
    key = cache_key,
    max_age = 7 * 24 * 60 * 60,
    refresh = refresh,
    function_to_run = function() {
      source_function <- getExportedValue("datasus", query$source_function)
      result <- do.call(source_function, source$arguments)
      if (!is.data.frame(result) || ncol(result) < 2L) {
        stop("A consulta TABNET não retornou uma tabela utilizável.", call. = FALSE)
      }
      list(
        data = result,
        provenance = datasus::datasus_proveniencia(result),
        original_headers = names(result),
        line = list(
          label = line_option$id[[1L]],
          value = line_option$value[[1L]]
        ),
        periods = periods,
        warnings = source$warnings,
        retrieved_at = Sys.time()
      )
    }
  )
}

parse_tabnet_number <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "...")] <- NA_character_
  x <- gsub(".", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

normalize_tabnet_result <- function(execution, dimension_kind) {
  data <- execution$data
  labels <- trimws(as.character(data[[1L]]))
  numeric_data <- as.data.frame(
    lapply(data[-1L], parse_tabnet_number),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (ncol(numeric_data) == 0L) {
    stop("A tabela TABNET não contém colunas numéricas.", call. = FALSE)
  }

  non_missing <- rowSums(!is.na(numeric_data))
  values <- rowSums(numeric_data, na.rm = TRUE)
  values[non_missing == 0L] <- NA_real_
  normalized_labels <- normalize_text(labels)

  result <- tibble::tibble(
    dimension = dimension_kind,
    label = labels,
    value = values,
    is_total = normalized_labels %in% c("total", "total_geral") |
      grepl("^total_", normalized_labels),
    source_line = execution$line$label
  )
  result <- result[nzchar(result$label) & !is.na(result$label), , drop = FALSE]
  attr(result, "original_headers") <- execution$original_headers
  result
}

tabnet_total <- function(normalized) {
  total <- normalized$value[normalized$is_total & !is.na(normalized$value)]
  if (length(total) > 0L) return(sum(total))
  sum(normalized$value[!normalized$is_total], na.rm = TRUE)
}

run_series_by_period <- function(query, geography_option, refresh = FALSE) {
  rows <- lapply(seq_len(nrow(query$periods)), function(index) {
    period <- query$periods[index, , drop = FALSE]
    execution <- execute_tabnet_query(
      query,
      geography_option,
      periods = period,
      purpose = paste0("series-period-", index),
      refresh = refresh
    )
    normalized <- normalize_tabnet_result(execution, "period")
    tibble::tibble(
      dimension = "period",
      label = period$id[[1L]],
      value = tabnet_total(normalized),
      is_total = FALSE,
      source_line = "Período solicitado"
    )
  })
  dplyr::bind_rows(rows)
}

filter_series_to_periods <- function(series, query) {
  selected <- normalize_text(query$periods$id)
  labels <- normalize_text(series$label)
  keep <- labels %in% selected
  if (any(keep)) return(series[keep, , drop = FALSE])
  if (query$frequency != "annual") {
    selected_months <- explicit_period_month(query$periods$id)
    series_months <- explicit_period_month(series$label)
    keep <- !is.na(series_months) & series_months %in% selected_months
    if (any(keep)) return(series[keep, , drop = FALSE])
  }
  series
}

run_tabnet_bundle <- function(query, refresh = FALSE) {
  validate_datasus_query(query)
  options <- query$options
  geography <- resolve_geography_dimension(options, query$geo_level)
  if (is.null(geography)) {
    available <- paste(options$linha$id, collapse = ", ")
    stop(
      "A fonte não oferece a geografia solicitada. Dimensões disponíveis: ",
      available,
      call. = FALSE
    )
  }

  map_periods <- if (query$frequency == "snapshot") {
    latest_period_row(query$periods)
  } else {
    query$periods
  }
  warnings <- if (query$frequency == "snapshot" && nrow(query$periods) > 1L) {
    "O mapa e o ranking do CNES usam apenas a última competência selecionada; a série preserva todos os estoques."
  } else {
    character()
  }

  map_execution <- execute_tabnet_query(
    query, geography, periods = map_periods, purpose = "map", refresh = refresh
  )
  map_data <- normalize_tabnet_result(map_execution, "geography")
  map_data <- map_data[!map_data$is_total, , drop = FALSE]
  warnings <- c(warnings, map_execution$warnings)

  ranking_option <- resolve_ranking_dimension(
    options,
    query$domain,
    exclude_values = geography$value
  )
  ranking_execution <- NULL
  ranking_kind <- "condition"
  if (is.null(ranking_option)) {
    ranking_data <- map_data
    ranking_kind <- "territory"
    warnings <- c(
      warnings,
      "A fonte não ofereceu dimensão de condição/procedimento; o ranking mostra territórios."
    )
  } else {
    ranking_result <- tryCatch(
      execute_tabnet_query(
        query, ranking_option, periods = map_periods, purpose = "ranking", refresh = refresh
      ),
      error = function(error) error
    )
    if (inherits(ranking_result, "error")) {
      ranking_data <- map_data
      ranking_kind <- "territory"
      warnings <- c(warnings, paste("Ranking por condição indisponível:", conditionMessage(ranking_result)))
    } else {
      ranking_execution <- ranking_result
      ranking_data <- normalize_tabnet_result(ranking_execution, "ranking")
      ranking_data <- ranking_data[!ranking_data$is_total, , drop = FALSE]
      warnings <- c(warnings, ranking_execution$warnings)
    }
  }

  time_option <- resolve_time_dimension(options, query$frequency)
  series_execution <- NULL
  if (is.null(time_option)) {
    series_data <- run_series_by_period(query, geography, refresh)
    warnings <- c(warnings, "A série foi construída com uma consulta separada por período.")
  } else {
    series_result <- tryCatch(
      execute_tabnet_query(
        query, time_option, periods = query$periods, purpose = "series", refresh = refresh
      ),
      error = function(error) error
    )
    if (inherits(series_result, "error")) {
      series_data <- run_series_by_period(query, geography, refresh)
      warnings <- c(
        warnings,
        paste("Dimensão temporal indisponível; série refeita por período:", conditionMessage(series_result))
      )
    } else {
      series_execution <- series_result
      series_data <- normalize_tabnet_result(series_execution, "period")
      series_data <- series_data[!series_data$is_total, , drop = FALSE]
      series_data <- filter_series_to_periods(series_data, query)
      warnings <- c(warnings, series_execution$warnings)
    }
  }

  provenance <- Filter(
    Negate(is.null),
    list(
      map = map_execution$provenance,
      ranking = ranking_execution$provenance %||% NULL,
      series = series_execution$provenance %||% NULL
    )
  )

  list(
    map = map_data,
    ranking = ranking_data,
    ranking_kind = ranking_kind,
    series = series_data,
    map_periods = map_periods,
    warnings = unique(warnings[nzchar(warnings)]),
    provenance = provenance,
    retrieved_at = Sys.time()
  )
}
