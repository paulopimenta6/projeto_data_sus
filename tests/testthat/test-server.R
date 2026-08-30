test_that("Analyze action propagates through the module boundary", {
  original_fetch_options <- fetch_datasus_options
  original_run_analysis <- run_analysis
  on.exit(assign("fetch_datasus_options", original_fetch_options, envir = globalenv()), add = TRUE)
  on.exit(assign("run_analysis", original_run_analysis, envir = globalenv()), add = TRUE)
  assign("fetch_datasus_options", function(...) mock_options(), envir = globalenv())
  assign("run_analysis", function(...) stop("analysis stub reached"), envir = globalenv())

  shiny::testServer(app_server, {
    session$setInputs(
      `filters-domain` = "sih_morbidade",
      `filters-dataset` = "geral_internacao",
      `filters-uf` = "AC",
      `filters-geo_level` = "municipality",
      `filters-scale` = "count",
      `filters-urgent_only` = FALSE
    )
    session$setInputs(`filters-load_options` = 1)
    session$flushReact()
    session$setInputs(
      `filters-measure` = "internacoes",
      `filters-periods` = "file2402.dbf",
      `filters-condition_field` = "",
      `filters-territory_field` = ""
    )
    session$setInputs(`filters-analyze` = 1)
    session$flushReact()

    expect_s3_class(analysis(), "datasus_analysis_error")
    expect_match(analysis()$message, "analysis stub reached", fixed = TRUE)
  })
})
