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

test_that("facility GeoJSON exports only public mapping fields", {
  facilities <- sf::st_as_sf(
    data.frame(
      co_cnes = "1234567",
      facility_name = "Hospital teste",
      name_muni = "Rio Branco",
      facility_type_code = "05",
      facility_date = 202604L,
      nu_cpf = "00000000000",
      longitude = -67.81,
      latitude = -9.97
    ),
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  write_facilities_geojson(facilities, path)
  exported <- sf::st_read(path, quiet = TRUE)
  expect_true(all(c("co_cnes", "facility_name", "name_muni") %in% names(exported)))
  expect_false("nu_cpf" %in% names(exported))
})

test_that("a one-period series uses a zero-based bar", {
  analysis <- list(
    series = tibble::tibble(label = "Mai/2026", display_value = 5328),
    metric_label = "Internações"
  )
  built <- ggplot2::ggplot_build(make_series_plot(analysis))
  expect_s3_class(built$plot$layers[[1L]]$geom, "GeomCol")
  expect_lte(built$layout$panel_scales_y[[1L]]$range$range[[1L]], 0)
})
