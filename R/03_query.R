new_datasus_query <- function(
  domain,
  dataset,
  uf = NULL,
  geo_level = "state",
  measure,
  periods,
  options,
  condition_field = NULL,
  condition_values = character(),
  territory_field = NULL,
  territory_values = character(),
  urgent_only = FALSE,
  scale = "count",
  comparison = "auto",
  map_method = "quantile",
  map_fixed_breaks = numeric(),
  top_n = 15L,
  interface_mode = "simple"
) {
  config <- get_domain_config(domain)
  if (identical(uf, "all") || identical(uf, "")) uf <- NULL

  if (!is.data.frame(periods) || !all(c("id", "value") %in% names(periods))) {
    stop("Os períodos devem conter rótulos e valores da fonte.", call. = FALSE)
  }
  if (nrow(periods) == 0L) stop("Selecione ao menos um período.", call. = FALSE)
  if (nrow(periods) > 120L) stop("Selecione no máximo 120 competências.", call. = FALSE)

  filters <- list()
  append_filter <- function(field, values) {
    if (is.null(field) || !nzchar(field) || length(values) == 0L) return(invisible(NULL))
    filters[[field]] <<- unique(c(filters[[field]] %||% character(), as.character(values)))
    invisible(NULL)
  }
  append_filter(condition_field, condition_values)
  append_filter(territory_field, territory_values)

  measure_spec <- list(
    id = as.character(measure$id[[1L]]),
    value = as.character(measure$value[[1L]]),
    measure_type = as.character(measure$measure_type[[1L]] %||% "unknown"),
    unit = as.character(measure$unit[[1L]] %||% "registros"),
    multiplier = as.numeric(measure$multiplier[[1L]] %||% 100000),
    rate_eligible = isTRUE(measure$rate_eligible[[1L]] %||% FALSE),
    standardizable = isTRUE(measure$standardizable[[1L]] %||% FALSE)
  )
  requested_scale <- match.arg(scale, c("count", "rate", "age_standardized_rate"))
  if (requested_scale == "rate" && !measure_spec$rate_eligible) {
    stop("A medida selecionada não aceita denominador populacional.", call. = FALSE)
  }
  if (requested_scale == "age_standardized_rate" && !measure_spec$standardizable) {
    stop("A medida selecionada não permite padronização direta por idade.", call. = FALSE)
  }

  query <- list(
    domain = domain,
    domain_label = config$label,
    system = config$system,
    provider = attr(options, "provider") %||% config$provider %||% "microdata",
    source_function = config$source_function,
    frequency = config$frequency,
    dataset = as.character(dataset),
    uf = if (is.null(uf)) NULL else toupper(as.character(uf)),
    geo_level = match.arg(geo_level, c("state", "municipality", "health_region")),
    measure_label = measure_spec$id,
    measure_value = measure_spec$value,
    measure_spec = measure_spec,
    periods = data.frame(
      id = as.character(periods$id),
      value = as.character(periods$value),
      stringsAsFactors = FALSE
    ),
    filters = filters,
    urgent_only = isTRUE(urgent_only),
    scale = requested_scale,
    comparison = match.arg(comparison, c("auto", "none", "state", "brazil")),
    map_method = match.arg(map_method, c("quantile", "equal", "log", "fixed")),
    map_fixed_breaks = sort(unique(as.numeric(map_fixed_breaks[is.finite(map_fixed_breaks)]))),
    top_n = match.arg(as.character(top_n), c("10", "15", "25")),
    interface_mode = match.arg(interface_mode, c("simple", "advanced")),
    options = options,
    created_at = Sys.time()
  )
  class(query) <- c("datasus_query", "list")
  validate_datasus_query(query)
  query
}

validate_datasus_query <- function(query) {
  required <- c(
    "domain", "system", "provider", "source_function", "dataset", "geo_level",
    "measure_value", "periods", "filters", "scale"
  )
  missing <- setdiff(required, names(query))
  if (length(missing) > 0L) {
    stop("Consulta incompleta: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  get_domain_config(query$domain)
  if (!nzchar(query$dataset)) stop("Conjunto de dados inválido.", call. = FALSE)
  if (!is.list(query$filters) || is.null(names(query$filters)) && length(query$filters) > 0L) {
    stop("Filtros devem formar uma lista nomeada.", call. = FALSE)
  }
  if (query$domain == "sim" && query$geo_level == "state" && !is.null(query$uf)) {
    stop(
      "No SIM agregado por UF, deixe a abrangência em Brasil; use municípios para detalhar uma UF.",
      call. = FALSE
    )
  }
  max_periods <- query$options$max_periods %||% 120L
  if (nrow(query$periods) > max_periods) {
    stop(
      "Este recorte permite no máximo ", max_periods,
      " período(s) por consulta de microdados.",
      call. = FALSE
    )
  }
  if (query$map_method == "fixed" && length(query$map_fixed_breaks) < 2L) {
    stop("Informe ao menos dois limites numéricos para a classificação fixa.", call. = FALSE)
  }
  invisible(query)
}

extract_year <- function(x) {
  x <- as.character(x)
  match <- regexpr("(19|20)[0-9]{2}", x, perl = TRUE)
  result <- rep(NA_integer_, length(x))
  has_match <- match > 0L
  result[has_match] <- as.integer(regmatches(x, match)[has_match])
  result
}

query_years <- function(query, periods = query$periods) {
  years <- extract_year(periods$id)
  missing <- is.na(years)
  years[missing] <- extract_year(periods$value[missing])
  sort(unique(stats::na.omit(years)))
}

explicit_period_month <- function(x) {
  unname(vapply(as.character(x), function(value) {
    year <- extract_year(value)
    year <- year[[1L]]
    if (is.na(year)) return(NA_integer_)
    normalized <- normalize_text(value)
    month_names <- c(
      jan = 1L, fev = 2L, mar = 3L, abr = 4L, mai = 5L, jun = 6L,
      jul = 7L, ago = 8L, set = 9L, out = 10L, nov = 11L, dez = 12L
    )
    month <- NA_integer_
    for (prefix in names(month_names)) {
      if (grepl(prefix, normalized, fixed = TRUE)) {
        month <- month_names[[prefix]]
        break
      }
    }
    if (is.na(month)) {
      numeric_match <- regexec(
        "(?:^|[^0-9])([01]?[0-9])[/_-](?:19|20)[0-9]{2}",
        value,
        perl = TRUE
      )
      pieces <- regmatches(value, numeric_match)[[1L]]
      if (length(pieces) >= 2L) month <- as.integer(pieces[[2L]])
    }
    if (is.na(month)) {
      compact_match <- regexec("((?:19|20)[0-9]{2})(0[1-9]|1[0-2])", value, perl = TRUE)
      pieces <- regmatches(value, compact_match)[[1L]]
      if (length(pieces) >= 3L) {
        year <- as.integer(pieces[[2L]])
        month <- as.integer(pieces[[3L]])
      }
    }
    if (is.na(month) || month < 1L || month > 12L) return(NA_integer_)
    year * 100L + month
  }, integer(1)))
}

latest_period_row <- function(periods) {
  period_text <- paste(periods$id, periods$value)
  month_key <- explicit_period_month(period_text)
  years <- extract_year(period_text)
  missing_month <- is.na(month_key) & !is.na(years)
  month_key[missing_month] <- years[missing_month] * 100L + 12L
  if (all(is.na(month_key))) return(periods[1L, , drop = FALSE])
  periods[which.max(month_key), , drop = FALSE]
}

default_period_row <- function(periods, frequency) {
  latest <- latest_period_row(periods)
  if (frequency != "monthly" || nrow(periods) < 2L) return(latest)

  latest_index <- match(latest$value[[1L]], periods$value)
  remaining <- periods[-latest_index, , drop = FALSE]
  latest_period_row(remaining)
}

query_period_weights <- function(query, periods = query$periods) {
  years <- extract_year(periods$id)
  missing <- is.na(years)
  years[missing] <- extract_year(periods$value[missing])
  years <- years[!is.na(years)]
  if (length(years) == 0L) {
    return(data.frame(year = integer(), weight = numeric()))
  }

  if (query$frequency == "monthly") {
    counts <- table(years)
    return(data.frame(
      year = as.integer(names(counts)),
      weight = as.numeric(counts) / 12,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(year = sort(unique(years)), weight = 1, stringsAsFactors = FALSE)
}

query_territory_codes <- function(query) {
  filter_index <- classify_source_filters(query$options %||% list())
  territory_fields <- intersect(
    filter_index$field[filter_index$role == "territory"],
    names(query$filters)
  )
  if (length(territory_fields) == 0L) return(character())

  values <- unique(unlist(query$filters[territory_fields], use.names = FALSE))
  values <- as.character(values)
  values <- values[nzchar(trimws(values)) & normalize_text(values) != "all"]
  if (length(values) == 0L) return(character())

  leading <- extract_leading_code(values, 6L, 7L)
  codes <- normalize_municipality_code(leading)
  if (any(is.na(codes))) {
    stop(
      "O filtro territorial contém código municipal ausente ou inválido.",
      call. = FALSE
    )
  }
  unique(codes)
}

query_to_manifest <- function(query) {
  list(
    domain = query$domain,
    domain_label = query$domain_label,
    system = query$system,
    provider = query$provider,
    dataset = query$dataset,
    uf = query$uf %||% "Brasil",
    geo_level = query$geo_level,
    measure = list(label = query$measure_label, value = query$measure_value),
    measure_spec = query$measure_spec,
    periods = unname(split(query$periods, seq_len(nrow(query$periods)))),
    filters = query$filters,
    urgent_only = query$urgent_only,
    scale = query$scale,
    comparison = query$comparison,
    map = list(
      method = query$map_method,
      fixed_breaks = query$map_fixed_breaks
    ),
    top_n = as.integer(query$top_n),
    interface_mode = query$interface_mode,
    created_at = format(query$created_at, "%Y-%m-%dT%H:%M:%S%z")
  )
}

is_count_measure <- function(measure) {
  if (!is.data.frame(measure) && !is.list(measure)) return(FALSE)
  type <- measure$measure_type %||% "unknown"
  isTRUE(as.character(type[[1L]]) %in% c("count", "amount"))
}

measure_rate_eligible <- function(measure) {
  if (!is.data.frame(measure) && !is.list(measure)) return(FALSE)
  isTRUE(measure$rate_eligible[[1L]] %||% FALSE)
}
