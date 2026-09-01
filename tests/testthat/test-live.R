test_that("live DATASUS microdata remains readable", {
  skip_if(Sys.getenv("RUN_LIVE_DATASUS_TESTS") != "true", "Set RUN_LIVE_DATASUS_TESTS=true")
  source_data <- tryCatch(
    {
      spec <- microdata_source_spec("sih_morbidade", "geral_internacao")
      parts <- data.frame(
        label = "Jan/2024", key = "202401", year = 2024L, month = 1L,
        stringsAsFactors = FALSE
      )
      fetch_microdata_slice(spec, parts, uf = "AC", refresh = TRUE)
    },
    error = function(error) error
  )
  if (inherits(source_data, "error")) {
    message <- conditionMessage(source_data)
    unavailable <- grepl(
      "timeout|failed|could not resolve|HTTP status|FTP|connection|transfer",
      message,
      ignore.case = TRUE
    )
    if (unavailable) skip(paste("DATASUS indisponível:", message))
    stop(source_data)
  }
  expect_true(source_data$provider %in% c("datasusr", "microdatasus"))
  expect_gt(nrow(source_data$data), 0L)
  expect_true(all(c("ANO_CMPT", "MES_CMPT", "MUNIC_MOV", "DIAG_PRINC") %in% names(source_data$data)))
})
