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

test_that("non-count measures are not eligible for rates", {
  expect_true(is_count_measure("Internações"))
  expect_true(is_count_measure("Qtd.aprovada"))
  expect_false(is_count_measure("Valor total"))
  expect_false(is_count_measure("Taxa mortalidade"))
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
