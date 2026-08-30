test_that("live DATASUS metadata remains readable", {
  skip_if(Sys.getenv("RUN_LIVE_DATASUS_TESTS") != "true", "Set RUN_LIVE_DATASUS_TESTS=true")
  previous <- options(datasus.timeout = 60, datasus.max_tries = 2)
  on.exit(options(previous), add = TRUE)
  source_options <- tryCatch(
    fetch_datasus_options("sim", "obitos", geo_level = "state", refresh = TRUE),
    error = function(error) error
  )
  if (inherits(source_options, "error")) {
    message <- conditionMessage(source_options)
    unavailable <- grepl(
      "TABNET request failed|timeout|failed to perform HTTP|could not resolve|HTTP status",
      message,
      ignore.case = TRUE
    )
    if (unavailable) skip(paste("DATASUS indisponível:", message))
    stop(source_options)
  }
  expect_s3_class(source_options, "datasus_opcoes")
  expect_true(all(
    c("linha", "conteudo", "periodo", "filtros") %in% names(source_options)
  ))
  expect_true(nrow(source_options$periodo) > 0L)
})
