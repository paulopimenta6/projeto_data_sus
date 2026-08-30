test_that("CSV and manifest exports are written", {
  analysis <- list(
    query = mock_query(),
    map_data = tibble::tibble(geo_code = "12", geo_name = "Acre", value = 10),
    series = tibble::tibble(label = "Jan/2024", value = 10),
    ranking = tibble::tibble(label = "Categoria", value = 10),
    manifest = list(application = APP_NAME)
  )
  map_path <- tempfile(fileext = ".csv")
  manifest_path <- tempfile(fileext = ".json")
  write_csv_utf8(export_map_table(analysis), map_path)
  write_manifest_json(analysis, manifest_path)
  expect_true(file.info(map_path)$size > 0)
  expect_true(jsonlite::validate(paste(readLines(manifest_path, warn = FALSE), collapse = "\n")))
})

test_that("application UI renders its main navigation", {
  rendered <- htmltools::renderTags(app_ui())$html
  expect_match(rendered, "Atlas de ocorrências e serviços", fixed = TRUE)
  expect_match(rendered, "Visão geral", fixed = TRUE)
  expect_match(rendered, "Dados e exportação", fixed = TRUE)
  expect_match(rendered, "Metodologia", fixed = TRUE)
})

test_that("choropleth widget is compatible with the installed Leaflet API", {
  polygon <- sf::st_polygon(list(matrix(
    c(-68, -10, -67, -10, -67, -9, -68, -9, -68, -10),
    ncol = 2,
    byrow = TRUE
  )))
  map_sf <- sf::st_sf(
    geo_name = "Território teste",
    display_value = 12.5,
    geometry = sf::st_sfc(polygon, crs = 4326)
  )
  analysis <- list(
    map_sf = map_sf,
    metric = "rate",
    metric_label = "Taxa por 100 mil"
  )
  expect_s3_class(build_choropleth_map(analysis), "leaflet")
})
