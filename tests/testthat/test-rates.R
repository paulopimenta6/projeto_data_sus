test_that("crude rates and exact Poisson intervals are calculated", {
  expect_equal(calculate_crude_rate(10, 10000), 100)
  interval <- poisson_rate_interval(10, 10000)
  expect_true(interval$lower < 100)
  expect_true(interval$upper > 100)
  expect_equal(poisson_rate_interval(0, 10000)$lower, 0)
})

test_that("invalid denominators remain missing", {
  expect_true(all(is.na(calculate_crude_rate(c(1, 1, NA), c(0, NA, 100)))))
})

test_that("direct age standardization is strict and returns gamma intervals", {
  events <- tibble::tibble(age_group = AGE_STANDARD_GROUPS, value = rep(10, 17))
  population <- tibble::tibble(
    age_group = AGE_STANDARD_GROUPS,
    person_years = rep(10000, 17)
  )
  standard <- stats::setNames(rep(1000, 17), AGE_STANDARD_GROUPS)
  result <- direct_age_standardize(events, population, standard)
  expect_true(result$complete)
  expect_equal(result$rate, 100)
  expect_lt(result$lower, result$rate)
  expect_gt(result$upper, result$rate)

  incomplete <- direct_age_standardize(events[-1L, ], population[-1L, ], standard)
  expect_false(incomplete$complete)
  expect_true(is.na(incomplete$rate))
})

test_that("DATASUS encoded ages use quinquennial groups", {
  expect_equal(decode_datasus_age(c(3001, 4001, 4080), "datasus_encoded"), c(0L, 1L, 80L))
  expect_equal(
    decode_datasus_age(c(10, 11, 42, 1), "sih_unit", c(2, 3, 4, 5)),
    c(0L, 0L, 42L, 101L)
  )
  expect_equal(age_group_quinquennial(c(0, 4, 5, 79, 80)), c("0-4", "0-4", "5-9", "75-79", "80+"))
})

test_that("non-count measures are not eligible for rates", {
  count <- list(measure_type = "count", rate_eligible = TRUE)
  amount <- list(measure_type = "amount", rate_eligible = TRUE)
  currency <- list(measure_type = "currency", rate_eligible = FALSE)
  expect_true(is_count_measure(count))
  expect_true(is_count_measure(amount))
  expect_false(is_count_measure(currency))
  expect_true(measure_rate_eligible(count))
  expect_false(measure_rate_eligible(currency))
  expect_false(is_count_measure("Internações"))
})

test_that("population source rules use SIDRA estimates and censuses", {
  original_collect <- collect_sidra_population
  on.exit(assign("collect_sidra_population", original_collect, envir = globalenv()), add = TRUE)
  calls <- new.env(parent = emptyenv())
  calls$values <- list()
  assign(
    "collect_sidra_population",
    function(year, table, variable, classific = "all", category = "all") {
      calls$values[[length(calls$values) + 1L]] <- list(
        year = year, table = table, variable = variable,
        classific = classific, category = category
      )
      tibble::tibble(geo_code = "1200401", year = as.integer(year), population = 1000)
    },
    envir = globalenv()
  )

  expect_equal(fetch_population_municipality(2010, "AC")$population, 1000)
  expect_equal(calls$values[[1L]]$table, 608)
  expect_equal(calls$values[[1L]]$classific, c("c1", "c2"))
  expect_equal(fetch_population_municipality(2021, "AC")$population, 1000)
  expect_equal(calls$values[[2L]]$table, 6579)
})

test_that("geographic population totals reject partial denominators", {
  query <- mock_query()
  query$geo_level <- "state"
  population <- tibble::tibble(
    geo_code = c("1200013", "1200054"),
    year = c(2024L, 2024L),
    population = c(1000, NA_real_)
  )
  result <- aggregate_population_geography(population, query, 2024L)
  expect_true(is.na(result$population))
})

test_that("regional population rejects an incomplete municipality crosswalk", {
  query <- mock_query()
  query$geo_level <- "health_region"
  population <- tibble::tibble(
    geo_code = c("1200013", "1200054"),
    year = c(2024L, 2024L),
    population = c(1000, 2000)
  )
  original_crosswalk <- load_health_region_crosswalk
  on.exit(assign("load_health_region_crosswalk", original_crosswalk, envir = globalenv()), add = TRUE)
  assign(
    "load_health_region_crosswalk",
    function(year, uf = NULL, refresh = FALSE) {
      tibble::tibble(municipality_code = "1200013", geo_code = "12001")
    },
    envir = globalenv()
  )
  expect_error(
    aggregate_population_geography(population, query, 2024L),
    "Nem todos os municípios"
  )
})

test_that("territorial filters restrict population before geographic aggregation", {
  query <- mock_query()
  query$geo_level <- "state"
  query$options$filter_roles <- c(municipio = "territory")
  query$filters$municipio <- "120001"
  original_fetch <- fetch_population_municipality
  on.exit(assign("fetch_population_municipality", original_fetch, envir = globalenv()), add = TRUE)
  assign(
    "fetch_population_municipality",
    function(year, uf = NULL, refresh = FALSE) {
      tibble::tibble(
        geo_code = c("1200013", "1200401"),
        year = as.integer(year),
        population = c(1000, 2000)
      )
    },
    envir = globalenv()
  )

  result <- fetch_population_panel(query)
  expect_equal(unique(result$data$geo_code), "12")
  expect_equal(unique(result$data$population), 1000)
})

test_that("health-region numerator and denominator share one reference vintage", {
  query <- mock_query()
  query$geo_level <- "health_region"
  query$periods <- data.frame(
    id = c("Jan/2022", "Jan/2024"),
    value = c("202201", "202401")
  )
  original_fetch <- fetch_population_municipality
  original_crosswalk <- load_health_region_crosswalk
  on.exit(assign("fetch_population_municipality", original_fetch, envir = globalenv()), add = TRUE)
  on.exit(assign("load_health_region_crosswalk", original_crosswalk, envir = globalenv()), add = TRUE)
  reference_years <- integer()
  assign(
    "fetch_population_municipality",
    function(year, uf = NULL, refresh = FALSE) {
      tibble::tibble(geo_code = "1200401", year = as.integer(year), population = 1000)
    },
    envir = globalenv()
  )
  assign(
    "load_health_region_crosswalk",
    function(year, uf = NULL, refresh = FALSE) {
      reference_years <<- c(reference_years, year)
      tibble::tibble(
        municipality_code = "1200401",
        geo_code = "12001",
        geo_name = "Baixo Acre e Purus"
      )
    },
    envir = globalenv()
  )

  fetch_population_panel(query)
  expect_equal(reference_years, c(2024L, 2024L))
})

test_that("population panels make absent territory-years explicit", {
  panel <- tibble::tibble(
    geo_code = "1200401",
    year = 2024L,
    population = 1000
  )
  weights <- data.frame(year = c(2024L, 2025L), weight = c(1, 1))
  result <- complete_population_panel(panel, "1200401", weights)
  expect_equal(nrow(result), 2L)
  expect_true(is.na(result$population[result$year == 2025L]))
})

test_that("conflicting duplicate population rows are not summed", {
  panel <- tibble::tibble(
    geo_code = c("1200401", "1200401"),
    year = c(2024L, 2024L),
    population = c(1000, 1100)
  )
  weights <- data.frame(year = 2024L, weight = 1)
  result <- complete_population_panel(panel, "1200401", weights)
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$population))
})

test_that("monthly map and series rates use consistent person-time", {
  query <- mock_query("rate")
  bundle <- list(
    map = tibble::tibble(geo_code = "1200401", value = 10),
    ranking = tibble::tibble(label = "Categoria", value = 10),
    series = tibble::tibble(label = c("Jan/2024", "Fev/2024"), value = c(4, 6)),
    map_periods = query$periods,
    warnings = character()
  )
  original_fetch <- fetch_population_panel
  on.exit(assign("fetch_population_panel", original_fetch, envir = globalenv()), add = TRUE)
  assign(
    "fetch_population_panel",
    function(query, periods, geo_codes = NULL, refresh = FALSE) {
      list(
        data = tibble::tibble(geo_code = "1200401", year = 2024L, population = 1200),
        warnings = character(),
        expected_years = 2024L
      )
    },
    envir = globalenv()
  )

  result <- enrich_bundle_with_rates(bundle, query)
  expect_equal(result$map$denominator, 200)
  expect_equal(result$series$denominator, c(100, 100))
  expect_equal(result$series$rate, c(4000, 6000))
})

test_that("rate denominators include territories with zero events", {
  query <- mock_query("rate")
  bundle <- list(
    map = tibble::tibble(
      geo_code = "1200401",
      geo_name = "Rio Branco",
      normalized_name = "rio branco",
      value = 10
    ),
    ranking = tibble::tibble(label = "Categoria", value = 10),
    series = tibble::tibble(label = c("Jan/2024", "Fev/2024"), value = c(4, 6)),
    map_periods = query$periods,
    warnings = character()
  )
  original_fetch <- fetch_population_panel
  original_universe <- microdata_geography_universe
  on.exit(assign("fetch_population_panel", original_fetch, envir = globalenv()), add = TRUE)
  on.exit(assign("microdata_geography_universe", original_universe, envir = globalenv()), add = TRUE)
  assign(
    "fetch_population_panel",
    function(query, periods, geo_codes = NULL, refresh = FALSE) {
      list(
        data = tibble::tibble(
          geo_code = c("1200013", "1200401"),
          year = 2024L,
          population = c(1200, 2400)
        ),
        warnings = character(),
        expected_years = 2024L
      )
    },
    envir = globalenv()
  )
  assign(
    "microdata_geography_universe",
    function(query, refresh = FALSE) {
      tibble::tibble(
        geo_code = c("1200013", "1200401"),
        geo_name = c("Acrelândia", "Rio Branco"),
        label = c("1200013 Acrelândia", "1200401 Rio Branco")
      )
    },
    envir = globalenv()
  )

  result <- enrich_bundle_with_rates(bundle, query)
  zero_row <- result$map[result$map$geo_code == "1200013", , drop = FALSE]
  expect_equal(zero_row$value, 0)
  expect_equal(zero_row$rate, 0)
  expect_equal(sum(result$map$denominator), 600)
})
