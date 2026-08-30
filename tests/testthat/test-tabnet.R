test_that("source filter roles include truncated urgency fields", {
  classified <- classify_source_filters(mock_options())
  expect_equal(classified$role[classified$field == "capitulo_cid_10"], "condition")
  expect_equal(classified$role[classified$field == "municipio"], "territory")
  expect_equal(classified$role[classified$field == "carater_atendiment"], "urgency")
})

test_that("dimension resolver selects geography and ranking", {
  options <- mock_options()
  expect_equal(resolve_geography_dimension(options, "health_region")$value, "regiao")
  expect_equal(resolve_ranking_dimension(options, "sih_morbidade")$value, "cid")
  expect_equal(resolve_time_dimension(options, "annual")$value, "ano")
})

test_that("TABNET numeric tables are normalized without double-counting totals", {
  execution <- list(
    data = data.frame(
      Local = c("11 Rondônia", "12 Acre", "Total"),
      Valor = c("1.234", "20", "1.254"),
      check.names = FALSE
    ),
    original_headers = c("Local", "Valor"),
    line = list(label = "UF", value = "uf")
  )
  result <- normalize_tabnet_result(execution, "geography")
  expect_equal(result$value, c(1234, 20, 1254))
  expect_equal(result$is_total, c(FALSE, FALSE, TRUE))
  expect_equal(tabnet_total(result), 1254)
})

test_that("monthly series excludes annual subtotal rows", {
  series <- tibble::tibble(label = c("2024", "Janeiro/2024", "Fevereiro/2024"))
  filtered <- filter_series_to_periods(series, mock_query())
  expect_equal(filtered$label, c("Janeiro/2024", "Fevereiro/2024"))
})

test_that("urgency option is added using the live field value", {
  result <- add_urgency_filter(list(), mock_options())
  expect_equal(result$filters$carater_atendiment, "02")
  expect_length(result$warnings, 0L)
})
