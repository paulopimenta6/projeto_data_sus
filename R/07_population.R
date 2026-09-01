find_column_by_name <- function(data, candidates) {
  normalized <- normalize_text(names(data))
  candidate_normalized <- normalize_text(candidates)
  for (candidate in candidate_normalized) {
    exact <- which(normalized == candidate)
    if (length(exact) > 0L) return(names(data)[exact[[1L]]])
  }
  for (candidate in candidate_normalized) {
    contains <- which(grepl(candidate, normalized, fixed = TRUE))
    if (length(contains) > 0L) return(names(data)[contains[[1L]]])
  }
  NULL
}

uf_code <- function(uf) {
  state_codes <- c(
    RO = "11", AC = "12", AM = "13", RR = "14", PA = "15", AP = "16", TO = "17",
    MA = "21", PI = "22", CE = "23", RN = "24", PB = "25", PE = "26", AL = "27",
    SE = "28", BA = "29", MG = "31", ES = "32", RJ = "33", SP = "35", PR = "41",
    SC = "42", RS = "43", MS = "50", MT = "51", GO = "52", DF = "53"
  )
  unname(state_codes[toupper(uf)])
}

collect_sidra_population <- function(
  year,
  table,
  variable,
  classific = "all",
  category = "all"
) {
  query <- sidrar::sidra_query(
    x = table,
    variable = variable,
    period = as.character(year),
    geo = "City",
    classific = classific,
    category = category,
    header = FALSE,
    format = 1,
    value_type = "both"
  )
  data <- sidrar::sidra_collect(query, provenance = TRUE)

  code_column <- find_column_by_name(data, c("D1C", "Município (Código)", "municipio_codigo"))
  value_column <- find_column_by_name(data, c("V", "Valor", "value"))
  if (is.null(code_column) || is.null(value_column)) {
    stop("A resposta SIDRA não contém código municipal e população.", call. = FALSE)
  }

  codes <- format_integer_code(data[[code_column]], 7L)
  values <- suppressWarnings(as.numeric(as.character(data[[value_column]])))
  result <- tibble::tibble(
    geo_code = codes,
    year = as.integer(year),
    population = values
  )
  result <- result[!is.na(result$geo_code), , drop = FALSE]
  if (anyDuplicated(result$geo_code)) {
    stop("A resposta SIDRA contém mais de uma população para o mesmo município.", call. = FALSE)
  }
  result
}

fetch_population_municipality <- function(year, uf = NULL, refresh = FALSE) {
  year <- as.integer(year)
  key <- list(
    sidrar_version = safe_package_version("sidrar"),
    source_rules_version = 2L,
    year = year,
    uf = uf
  )

  cached_call(
    namespace = "population",
    key = key,
    max_age = 365 * 24 * 60 * 60,
    refresh = refresh,
    function_to_run = function() {
      if (year == 2000L) {
        result <- collect_sidra_population(
          year, table = 202, variable = 93,
          classific = c("c1", "c2"), category = list(0, 0)
        )
      } else if (year == 2010L) {
        result <- collect_sidra_population(
          year, table = 608, variable = 93,
          classific = c("c1", "c2"), category = list(0, 0)
        )
      } else if (year == 2022L) {
        result <- collect_sidra_population(year, table = 4709, variable = 93)
      } else if (year == 2023L) {
        stop(
          "Não há estimativa municipal anual oficial na tabela SIDRA 6579 para 2023.",
          call. = FALSE
        )
      } else if (year >= 2001L) {
        unavailable <- c(2007L)
        if (year %in% unavailable) {
          stop("Não há estimativa municipal anual na tabela SIDRA 6579 para ", year, ".", call. = FALSE)
        }
        result <- collect_sidra_population(year, table = 6579, variable = 9324)
      } else {
        stop("Não há denominador municipal SIDRA configurado para ", year, ".", call. = FALSE)
      }
      if (!is.null(uf)) result <- result[substr(result$geo_code, 1L, 2L) == uf_code(uf), , drop = FALSE]
      result
    }
  )
}

load_health_region_crosswalk <- function(year, uf = NULL, refresh = FALSE) {
  geometry_year <- choose_available_year(year, "health_region")
  key <- list(
    geobr_version = safe_package_version("geobr"),
    geometry_year = geometry_year,
    uf = uf
  )
  crosswalk <- cached_call(
    namespace = "health-region-crosswalk",
    key = key,
    max_age = 365 * 24 * 60 * 60,
    refresh = refresh,
    function_to_run = function() {
      suppressMessages(
        geobr::read_health_region(
          year = geometry_year,
          code_state = uf %||% "all",
          geometry_level = "municipality",
          simplified = TRUE,
          output = "sf",
          showProgress = FALSE,
          cache = TRUE,
          verbose = FALSE
        )
      )
    }
  )
  tibble::tibble(
    municipality_code = format_integer_code(crosswalk$code_muni, 7L),
    geo_code = format_integer_code(crosswalk$code_health_region),
    geo_name = as.character(crosswalk$name_health_region)
  )
}

aggregate_population_geography <- function(
  population,
  query,
  year,
  refresh = FALSE,
  reference_year = analysis_reference_year(query)
) {
  if (query$geo_level == "municipality") return(population)
  if (any(is.na(population$geo_code))) {
    stop("A população contém código municipal ausente ou inválido.", call. = FALSE)
  }

  if (query$geo_level == "state") {
    population$geo_code <- substr(population$geo_code, 1L, 2L)
  } else {
    crosswalk <- load_health_region_crosswalk(reference_year, query$uf, refresh)
    if (anyDuplicated(crosswalk$municipality_code)) {
      stop("A malha de regiões de saúde contém municípios duplicados.", call. = FALSE)
    }
    population <- dplyr::left_join(
      dplyr::rename(population, municipality_code = "geo_code"),
      crosswalk,
      by = "municipality_code"
    )
    if (any(is.na(population$geo_code))) {
      stop(
        "Nem todos os municípios possuem uma região de saúde compatível.",
        call. = FALSE
      )
    }
  }

  population <- dplyr::group_by(population, .data$geo_code, .data$year)
  dplyr::summarise(
    population,
    population = if (any(is.na(.data$population))) NA_real_ else sum(.data$population),
    .groups = "drop"
  )
}

fetch_population_panel <- function(query, periods = query$periods, geo_codes = NULL, refresh = FALSE) {
  weights <- query_period_weights(query, periods)
  territory_codes <- query_territory_codes(query)
  reference_year <- analysis_reference_year(query)
  warnings <- character()
  records <- lapply(seq_len(nrow(weights)), function(index) {
    year <- weights$year[[index]]
    result <- tryCatch(
      {
        population <- fetch_population_municipality(year, query$uf, refresh)
        if (length(territory_codes) > 0L) {
          population <- population[
            population$geo_code %in% territory_codes,
            ,
            drop = FALSE
          ]
        }
        aggregate_population_geography(
          population, query, year, refresh,
          reference_year = reference_year
        )
      },
      error = function(error) error
    )
    if (inherits(result, "error")) {
      warnings <<- c(warnings, paste0("População de ", year, " indisponível: ", conditionMessage(result)))
      return(NULL)
    }
    result
  })
  panel <- dplyr::bind_rows(records)
  if (nrow(panel) == 0L) {
    return(list(data = panel, warnings = warnings, expected_years = weights$year))
  }

  panel <- dplyr::left_join(panel, weights, by = "year")
  panel$person_years <- panel$population * panel$weight
  if (!is.null(geo_codes) && length(geo_codes) > 0L) {
    panel <- panel[panel$geo_code %in% geo_codes, , drop = FALSE]
  }
  list(data = panel, warnings = warnings, expected_years = weights$year)
}

calculate_crude_rate <- function(events, denominator, multiplier = 100000) {
  ifelse(
    is.na(events) | is.na(denominator) | denominator <= 0,
    NA_real_,
    events / denominator * multiplier
  )
}

poisson_rate_interval <- function(events, denominator, confidence = 0.95, multiplier = 100000) {
  valid <- !is.na(events) & !is.na(denominator) & denominator > 0 & events >= 0 &
    abs(events - round(events)) < sqrt(.Machine$double.eps)
  lower <- upper <- rep(NA_real_, length(events))
  alpha <- 1 - confidence
  event_count <- round(events[valid])
  lower[valid] <- ifelse(
    event_count == 0,
    0,
    0.5 * stats::qchisq(alpha / 2, 2 * event_count) / denominator[valid] * multiplier
  )
  upper[valid] <- 0.5 * stats::qchisq(
    1 - alpha / 2,
    2 * (event_count + 1)
  ) / denominator[valid] * multiplier
  list(lower = lower, upper = upper)
}

add_rate_columns <- function(data, denominator_column = "denominator") {
  denominator <- data[[denominator_column]]
  data$rate <- calculate_crude_rate(data$value, denominator)
  interval <- poisson_rate_interval(data$value, denominator)
  data$rate_low <- interval$lower
  data$rate_high <- interval$upper
  data
}

match_series_years <- function(series, query) {
  years <- extract_year(series$label)
  if (any(is.na(years))) {
    period_lookup <- stats::setNames(
      extract_year(query$periods$id),
      normalize_text(query$periods$id)
    )
    lookup <- unname(period_lookup[normalize_text(series$label)])
    years[is.na(years)] <- lookup[is.na(years)]
  }
  years
}

complete_population_panel <- function(panel, geo_codes, weights) {
  geo_codes <- unique(stats::na.omit(as.character(geo_codes)))
  if (length(geo_codes) == 0L || nrow(weights) == 0L) {
    return(tibble::tibble(
      geo_code = character(), year = integer(), population = numeric(),
      weight = numeric(), person_years = numeric()
    ))
  }

  expected <- tidyr::expand_grid(
    geo_code = geo_codes,
    year = as.integer(weights$year)
  )
  required <- c("geo_code", "year", "population")
  population <- if (all(required %in% names(panel))) {
    population <- dplyr::select(panel, dplyr::all_of(required))
    population <- dplyr::group_by(population, .data$geo_code, .data$year)
    dplyr::summarise(
      population,
      population = if (dplyr::n_distinct(.data$population) == 1L) {
        dplyr::first(.data$population)
      } else {
        NA_real_
      },
      .groups = "drop"
    )
  } else {
    tibble::tibble(geo_code = character(), year = integer(), population = numeric())
  }
  completed <- dplyr::left_join(expected, population, by = c("geo_code", "year"))
  completed <- dplyr::left_join(completed, weights, by = "year")
  completed$person_years <- completed$population * completed$weight
  completed
}

complete_geographic_rows <- function(data, geo_codes, query, refresh = FALSE) {
  geo_codes <- unique(stats::na.omit(as.character(geo_codes)))
  missing_codes <- setdiff(geo_codes, data$geo_code)
  if (length(missing_codes) == 0L) return(data)

  names <- stats::setNames(character(), character())
  if (identical(query$provider, "microdata")) {
    universe <- microdata_geography_universe(query, refresh)
    names <- stats::setNames(universe$geo_name, universe$geo_code)
  }
  missing_names <- unname(names[missing_codes])
  missing_names[is.na(missing_names) | !nzchar(missing_names)] <- missing_codes[
    is.na(missing_names) | !nzchar(missing_names)
  ]
  dplyr::bind_rows(
    data,
    tibble::tibble(
      geo_code = missing_codes,
      geo_name = missing_names,
      normalized_name = normalize_text(missing_names),
      value = 0
    )
  )
}

enrich_bundle_with_rates <- function(bundle, query, refresh = FALSE) {
  if (!is_count_measure(query$measure_label)) {
    bundle$warnings <- unique(c(
      bundle$warnings,
      "Taxas não foram calculadas porque a medida selecionada não representa uma contagem."
    ))
    return(bundle)
  }

  map_panel_result <- fetch_population_panel(
    query,
    periods = bundle$map_periods,
    refresh = refresh
  )
  map_codes <- union(
    unique(stats::na.omit(bundle$map$geo_code)),
    unique(stats::na.omit(map_panel_result$data$geo_code))
  )
  bundle$map <- complete_geographic_rows(bundle$map, map_codes, query, refresh)
  map_weights <- query_period_weights(query, bundle$map_periods)
  map_panel <- complete_population_panel(map_panel_result$data, map_codes, map_weights)
  bundle$warnings <- unique(c(bundle$warnings, map_panel_result$warnings))

  map_denominator <- dplyr::group_by(map_panel, .data$geo_code)
  map_denominator <- dplyr::summarise(
    map_denominator,
    denominator = if (any(is.na(.data$person_years))) NA_real_ else sum(.data$person_years),
    .groups = "drop"
  )
  bundle$map <- dplyr::left_join(bundle$map, map_denominator, by = "geo_code")
  bundle$map <- add_rate_columns(bundle$map)

  series_panel_result <- fetch_population_panel(
    query,
    periods = query$periods,
    refresh = refresh
  )
  series_codes <- union(
    map_codes,
    unique(stats::na.omit(series_panel_result$data$geo_code))
  )
  series_weights <- query_period_weights(query, query$periods)
  series_panel <- complete_population_panel(
    series_panel_result$data,
    series_codes,
    series_weights
  )
  bundle$warnings <- unique(c(bundle$warnings, series_panel_result$warnings))

  scope_population <- dplyr::group_by(series_panel, .data$year)
  scope_population <- dplyr::summarise(
    scope_population,
    denominator = if (any(is.na(.data$population))) NA_real_ else sum(.data$population),
    .groups = "drop"
  )
  if (query$frequency == "monthly") {
    scope_population$denominator <- scope_population$denominator / 12
  }
  bundle$series$year <- match_series_years(bundle$series, query)
  bundle$series <- dplyr::left_join(bundle$series, scope_population, by = "year")
  bundle$series <- add_rate_columns(bundle$series)

  ranking_denominator <- if (nrow(map_panel) > 0L && all(!is.na(map_panel$person_years))) {
    sum(map_panel$person_years)
  } else {
    NA_real_
  }
  bundle$ranking$denominator <- ranking_denominator
  bundle$ranking <- add_rate_columns(bundle$ranking)

  missing_map <- sum(is.na(bundle$map$denominator))
  if (missing_map > 0L) {
    bundle$warnings <- unique(c(
      bundle$warnings,
      paste0(missing_map, " território(s) ficaram sem denominador populacional completo.")
    ))
  }
  bundle$population <- series_panel
  bundle
}
