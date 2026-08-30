test_that("DATASUS municipality codes become seven-digit IBGE codes", {
  expect_equal(normalize_municipality_code(c("355030", "3550308")), c("3550308", "3550308"))
})

test_that("geographic labels retain code, name, and totals", {
  data <- tibble::tibble(
    label = c("11 Rondônia", "12 Acre"),
    value = c(10, 20)
  )
  result <- normalize_geographic_data(data, "state")
  expect_equal(result$geo_code, c("11", "12"))
  expect_equal(result$geo_name, c("Rondônia", "Acre"))
  expect_equal(result$value, c(10, 20))
})

test_that("geometry vintages use the nearest prior available year", {
  expect_equal(choose_available_year(2022, "health_region"), 2013)
  expect_equal(choose_available_year(2026, "municipality"), 2025)
})

test_that("facility snapshot selection is explicit", {
  query <- mock_query()
  query$periods <- data.frame(id = "Fev/2024", value = "x")
  expect_equal(choose_facility_date(query), 202401)
})
