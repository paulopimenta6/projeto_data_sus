GEOMETRY_YEARS <- list(
  state = c(1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970, 1980, 1991, 2000, 2001, 2010, 2013:2025),
  municipality = c(
    1872, 1900, 1911, 1920, 1933, 1940, 1950, 1960, 1970,
    1980, 1991, 2000, 2001, 2005, 2007, 2010, 2013:2025
  ),
  health_region = c(1991, 1994, 1997, 2001, 2005, 2013, 2023, 2024, 2025)
)

FACILITY_DATES <- c(
  201704, 201707, 201710,
  as.vector(outer(2018:2025, c(1, 4, 7, 10), function(year, month) year * 100 + month)),
  202601, 202604
)
FACILITY_DATES <- sort(unique(as.integer(FACILITY_DATES)))

FACILITY_TYPE_CODES <- list(
  all = character(),
  hospital = c("05", "07", "15", "62"),
  emergency = c("09", "12", "20", "21", "42", "73", "76"),
  primary_care = c("01", "02", "45")
)

facility_type_choices <- function() {
  c(
    "Todos os estabelecimentos" = "all",
    "Hospitais e unidades mistas" = "hospital",
    "Urgência, emergência e pronto atendimento" = "emergency",
    "Atenção primária, UBS e saúde da família" = "primary_care"
  )
}

extract_leading_code <- function(x, min_digits = 2L, max_digits = 7L) {
  pattern <- paste0("^\\s*([0-9]{", min_digits, ",", max_digits, "})(?:\\s|$|[-–])")
  matched <- regexec(pattern, as.character(x), perl = TRUE)
  pieces <- regmatches(as.character(x), matched)
  vapply(pieces, function(piece) if (length(piece) >= 2L) piece[[2L]] else NA_character_, character(1))
}

format_integer_code <- function(x, width = NULL) {
  x <- as.character(x)
  x <- sub("[.]0+$", "", x)
  x[!grepl("^[0-9]+$", x)] <- NA_character_
  if (!is.null(width)) {
    valid <- !is.na(x)
    x[valid] <- sprintf(paste0("%0", width, "d"), as.integer(x[valid]))
  }
  x
}

normalize_municipality_code <- function(code) {
  code <- format_integer_code(code)
  result <- rep(NA_character_, length(code))
  seven <- !is.na(code) & nchar(code) == 7L
  result[seven] <- code[seven]
  six <- !is.na(code) & nchar(code) == 6L
  if (any(six)) {
    normalized <- tryCatch(
      datasus::normalizar_codigo_ibge(code[six], desconhecido = "na"),
      error = function(error) rep(NA_character_, sum(six))
    )
    result[six] <- format_integer_code(normalized, width = 7L)
  }
  result
}

normalize_geographic_data <- function(data, geo_level) {
  leading <- switch(
    geo_level,
    state = extract_leading_code(data$label, 2L, 2L),
    municipality = extract_leading_code(data$label, 6L, 7L),
    health_region = extract_leading_code(data$label, 2L, 7L)
  )
  code <- switch(
    geo_level,
    state = format_integer_code(leading, 2L),
    municipality = normalize_municipality_code(leading),
    health_region = format_integer_code(leading)
  )
  name <- trimws(sub("^\\s*[0-9]{2,7}\\s*[-–]?\\s*", "", data$label, perl = TRUE))
  name[!nzchar(name)] <- data$label[!nzchar(name)]

  normalized <- dplyr::mutate(
    data,
    geo_code = code,
    geo_name = name,
    normalized_name = normalize_text(name)
  )
  normalized <- dplyr::group_by(normalized, .data$geo_code, .data$geo_name, .data$normalized_name)
  normalized <- dplyr::summarise(
    normalized,
    value = if (all(is.na(.data$value))) NA_real_ else sum(.data$value, na.rm = TRUE),
    .groups = "drop"
  )
  normalized
}

choose_available_year <- function(requested_year, geography) {
  available <- GEOMETRY_YEARS[[geography]]
  if (is.null(available)) stop("Geografia desconhecida.", call. = FALSE)
  requested_year <- as.integer(requested_year)
  before <- available[available <= requested_year]
  if (length(before) > 0L) return(max(before))
  min(available)
}

analysis_reference_year <- function(query, periods = query$periods) {
  years <- query_years(query, periods)
  if (length(years) == 0L) return(max(GEOMETRY_YEARS[[query$geo_level]]))
  max(years)
}

load_geography <- function(query, periods = query$periods, refresh = FALSE) {
  requested <- analysis_reference_year(query, periods)
  geometry_year <- choose_available_year(requested, query$geo_level)
  key <- list(
    geobr_version = safe_package_version("geobr"),
    geo_level = query$geo_level,
    geometry_year = geometry_year,
    uf = query$uf
  )

  shape <- cached_call(
    namespace = "geometries",
    key = key,
    max_age = 365 * 24 * 60 * 60,
    refresh = refresh,
    function_to_run = function() {
      common <- list(
        year = geometry_year,
        simplified = TRUE,
        output = "sf",
        showProgress = FALSE,
        cache = TRUE,
        verbose = FALSE
      )
      suppressMessages(
        switch(
          query$geo_level,
          state = do.call(geobr::read_state, common),
          municipality = do.call(
            geobr::read_municipality,
            c(common, list(code_muni = query$uf %||% "all"))
          ),
          health_region = do.call(
            geobr::read_health_region,
            c(common, list(
              code_state = query$uf %||% "all",
              geometry_level = "micro"
            ))
          )
        )
      )
    }
  )

  if (!inherits(shape, "sf")) stop("A geometria não foi carregada como objeto sf.", call. = FALSE)
  shape <- switch(
    query$geo_level,
    state = dplyr::mutate(
      shape,
      geo_code = format_integer_code(.data$code_state, 2L),
      geo_name = as.character(.data$name_state)
    ),
    municipality = dplyr::mutate(
      shape,
      geo_code = format_integer_code(.data$code_muni, 7L),
      geo_name = as.character(.data$name_muni)
    ),
    health_region = dplyr::mutate(
      shape,
      geo_code = format_integer_code(.data$code_health_region),
      geo_name = as.character(.data$name_health_region)
    )
  )
  shape$normalized_name <- normalize_text(shape$geo_name)
  list(shape = shape, geometry_year = geometry_year)
}

join_analysis_geometry <- function(map_data, query, periods = query$periods, refresh = FALSE) {
  geography <- load_geography(query, periods, refresh)
  shape <- geography$shape
  value_columns <- setdiff(names(map_data), c("geo_code", "geo_name", "normalized_name"))

  joined <- dplyr::left_join(
    shape,
    dplyr::select(map_data, -dplyr::any_of(c("geo_name", "normalized_name"))),
    by = "geo_code"
  )

  unmatched_shape <- is.na(joined$value)
  fallback_match <- match(joined$normalized_name[unmatched_shape], map_data$normalized_name)
  fallback_rows <- which(unmatched_shape)
  valid_fallback <- !is.na(fallback_match)
  if (any(valid_fallback)) {
    for (column in value_columns) {
      if (!column %in% names(joined)) next
      rows <- fallback_rows[valid_fallback]
      joined[[column]][rows] <- map_data[[column]][fallback_match[valid_fallback]]
    }
  }

  matched_codes <- unique(joined$geo_code[!is.na(joined$value)])
  unmatched_data <- map_data[
    !is.na(map_data$geo_code) & !(map_data$geo_code %in% matched_codes),
    ,
    drop = FALSE
  ]

  list(
    data = joined,
    geometry_year = geography$geometry_year,
    unmatched = unmatched_data
  )
}

period_to_year_month <- function(period_label, period_value = period_label) {
  text <- paste(period_label, period_value)
  year <- extract_year(text)
  year <- year[[1L]]
  if (is.na(year)) return(NA_integer_)

  normalized <- normalize_text(text)
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
    numeric_match <- regexec("(?:^|[^0-9])([01]?[0-9])[/_-](?:19|20)[0-9]{2}", text, perl = TRUE)
    pieces <- regmatches(text, numeric_match)[[1L]]
    if (length(pieces) >= 2L) month <- as.integer(pieces[[2L]])
  }
  if (is.na(month) || month < 1L || month > 12L) month <- 1L
  year * 100L + month
}

choose_facility_date <- function(query) {
  period <- latest_period_row(query$periods)
  requested <- period_to_year_month(period$id[[1L]], period$value[[1L]])
  if (is.na(requested)) return(max(FACILITY_DATES))
  before <- FACILITY_DATES[FACILITY_DATES <= requested]
  if (length(before) > 0L) return(max(before))
  min(FACILITY_DATES)
}

load_health_facilities <- function(query, facility_type = "all", refresh = FALSE, max_points = 40000L) {
  facility_type <- match.arg(facility_type, names(FACILITY_TYPE_CODES))
  if (is.null(query$uf)) {
    stop("Para o mapa de pontos, selecione uma UF para evitar mais de 600 mil registros.", call. = FALSE)
  }
  facility_date <- choose_facility_date(query)
  key <- list(
    geobr_version = safe_package_version("geobr"),
    date = facility_date,
    uf = query$uf,
    facility_type = facility_type
  )

  facilities <- cached_call(
    namespace = "health-facilities",
    key = key,
    max_age = 30 * 24 * 60 * 60,
    refresh = refresh,
    function_to_run = function() {
      suppressMessages(
        geobr::read_health_facilities(
          date = facility_date,
          code_muni = query$uf,
          output = "sf",
          showProgress = FALSE,
          cache = TRUE,
          verbose = FALSE
        )
      )
    }
  )

  if (!inherits(facilities, "sf")) stop("Estabelecimentos não retornaram geometria sf.", call. = FALSE)
  type_column <- if ("tp_unidade" %in% names(facilities)) "tp_unidade" else "co_tipo_unidade"
  type_code <- format_integer_code(facilities[[type_column]], 2L)
  requested_codes <- FACILITY_TYPE_CODES[[facility_type]]
  keep_type <- length(requested_codes) == 0L | type_code %in% requested_codes
  keep_active <- if ("co_motivo_desab" %in% names(facilities)) {
    is.na(facilities$co_motivo_desab) | !nzchar(trimws(as.character(facilities$co_motivo_desab)))
  } else {
    rep(TRUE, nrow(facilities))
  }

  coordinates <- sf::st_coordinates(facilities)
  keep_coordinates <- is.finite(coordinates[, 1L]) & is.finite(coordinates[, 2L]) &
    coordinates[, 1L] >= -75 & coordinates[, 1L] <= -32 &
    coordinates[, 2L] >= -35 & coordinates[, 2L] <= 6
  facilities <- facilities[keep_type & keep_active & keep_coordinates, , drop = FALSE]
  facilities$facility_type_code <- type_code[keep_type & keep_active & keep_coordinates]

  if (nrow(facilities) > max_points) {
    stop(
      "O resultado possui ", format_pt_number(nrow(facilities)),
      " pontos. Restrinja o tipo de estabelecimento ou o território.",
      call. = FALSE
    )
  }

  name_column <- if ("no_fantasia" %in% names(facilities)) "no_fantasia" else "no_razao_social"
  facilities$facility_name <- as.character(facilities[[name_column]])
  facilities$facility_date <- facility_date
  facilities
}
