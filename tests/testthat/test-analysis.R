test_that("analysis composes normalized data, summary, and manifest", {
  query <- mock_query()
  bundle <- list(
    map = tibble::tibble(
      dimension = "geography",
      label = c("120040 RIO BRANCO", "120020 CRUZEIRO DO SUL"),
      value = c(100, 40),
      is_total = FALSE,
      source_line = "Município"
    ),
    ranking = tibble::tibble(
      dimension = "ranking",
      label = paste("Doença", seq_len(25L)),
      value = seq.int(250, 10, by = -10),
      is_total = FALSE,
      source_line = "CID"
    ),
    ranking_kind = "condition",
    series = tibble::tibble(
      dimension = "period",
      label = c("Janeiro/2024", "Fevereiro/2024"),
      value = c(60, 80),
      is_total = FALSE,
      source_line = "Competência"
    ),
    map_periods = query$periods,
    warnings = character(),
    provenance = list(),
    retrieved_at = Sys.time()
  )
  original_run_bundle <- run_datasus_bundle
  original_join_geometry <- join_analysis_geometry
  on.exit(assign("run_datasus_bundle", original_run_bundle, envir = globalenv()), add = TRUE)
  on.exit(assign("join_analysis_geometry", original_join_geometry, envir = globalenv()), add = TRUE)
  assign("run_datasus_bundle", function(query, refresh = FALSE) bundle, envir = globalenv())
  assign(
    "join_analysis_geometry",
    function(map_data, query, periods, refresh = FALSE) {
      list(data = NULL, geometry_year = 2024L, unmatched = NULL)
    },
    envir = globalenv()
  )

  result <- run_analysis(query)
  expect_s3_class(result, "datasus_analysis")
  expect_equal(result$summary$total, 140)
  expect_equal(result$summary$top_name, "RIO BRANCO")
  expect_equal(result$summary$latest_period, "Fev/2024")
  expect_equal(result$manifest$result$geometry_year, 2024L)
  expect_equal(nrow(result$ranking), 25L)
})

test_that("missing map values are not summarized as zero", {
  query <- mock_query()
  bundle <- list(
    map = tibble::tibble(value = NA_real_, display_value = NA_real_),
    total = NA_real_,
    metric = "value",
    metric_label = "Internações"
  )
  summary <- summarize_analysis(bundle, query)
  expect_true(is.na(summary$total))
})

test_that("overall rates require complete matching denominators", {
  query <- mock_query("rate")
  bundle <- list(
    map = tibble::tibble(
      geo_name = c("A", "B"),
      value = c(10, 20),
      denominator = c(1000, NA_real_),
      display_value = c(1000, NA_real_)
    ),
    total = 30,
    metric = "rate",
    metric_label = "Taxa por 100 mil pessoas-ano"
  )
  summary <- summarize_analysis(bundle, query)
  expect_true(is.na(summary$overall_rate))
})
