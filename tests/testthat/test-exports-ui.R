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

test_that("map classification remains useful with outliers, zeros, and missing values", {
  values <- c(0, 1, 2, 3, 10, 50, 100, 10000, NA)
  classification <- classify_map_values(values, "quantile")
  expect_equal(classification$method, "quantile")
  expect_gte(classification$bins, 2L)
  expect_equal(classification$zero_count, 1L)
  expect_equal(classification$missing_count, 1L)

  all_zero <- classify_map_values(c(0, 0, NA), "log")
  expect_equal(all_zero$breaks, c(0, 1))
})

test_that("fixed map classification validates its limits", {
  expect_error(classify_map_values(1:3, "fixed", 1), "ao menos dois limites")
  fixed <- classify_map_values(1:3, "fixed", c(0, 2, 4))
  expect_equal(fixed$breaks, c(0, 2, 4))
})

test_that("the audit report renders as a self-contained HTML file", {
  skip_if_not(rmarkdown::pandoc_available())
  query <- mock_query()
  analysis <- list(
    query = query,
    summary = list(
      total = 18, overall_rate = NA_real_, metric = "count",
      top_name = "Rio Branco", top_value = 12,
      territories_with_data = 2L
    ),
    series = tibble::tibble(
      label = c("Jan/2024", "Fev/2024"),
      display_value = c(8, 10)
    ),
    ranking = tibble::tibble(
      label = c("Rio Branco", "Cruzeiro do Sul"),
      display_value = c(12, 6)
    ),
    ranking_kind = "territory",
    metric = "count",
    metric_label = "Internações",
    geography_semantics = "Município de internação/processamento",
    map_data = tibble::tibble(
      geo_code = c("1200401", "1200203"),
      geo_name = c("Rio Branco", "Cruzeiro do Sul"),
      value = c(12, 6)
    ),
    map_sf = NULL,
    geometry_year = NA_integer_,
    classification = list(method = "quantile", method_label = "Quantis"),
    insights = list(),
    quality = tibble::tibble(
      indicator = c("territorios_validos", "territorios_sem_dado"),
      value = c(2, 0)
    ),
    manifest = list(schema_version = ANALYSIS_MANIFEST_VERSION)
  )
  path <- tempfile(fileext = ".html")
  on.exit(unlink(path), add = TRUE)

  write_analysis_report(analysis, path)

  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 10000)
  html <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "Relatório auditável DATASUS", fixed = TRUE)
})

test_that("compact charts avoid duplicate labels and crowded time axes", {
  labels <- paste(
    "Categoria com descrição epidemiológica extensa e informativa",
    seq_len(15L)
  )
  analysis <- list(
    query = list(top_n = 15L),
    series = tibble::tibble(label = paste0("P", seq_len(24L)), display_value = seq_len(24L)),
    ranking = tibble::tibble(label = labels, display_value = rev(seq_len(15L))),
    ranking_kind = "condition",
    metric_label = "Internações",
    geography_semantics = "Município de internação"
  )

  series <- ggplot2::ggplot_build(make_series_plot(analysis, compact = TRUE))
  ranking <- ggplot2::ggplot_build(make_ranking_plot(analysis, compact = TRUE))
  series_breaks <- series$layout$panel_params[[1L]]$x$breaks
  ranking_labels <- ranking$layout$panel_params[[1L]]$y$get_labels()

  expect_lte(sum(!is.na(series_breaks)), 5L)
  expect_length(unique(ranking_labels), 15L)
  expect_false(any(grepl("\n", ranking_labels, fixed = TRUE)))
})
