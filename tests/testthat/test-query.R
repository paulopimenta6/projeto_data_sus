test_that("text and period normalization is deterministic", {
  expect_equal(normalize_text(c("Região de Saúde", "ÓBITOS")), c("regiao_de_saude", "obitos"))
  expect_equal(extract_year(c("Jan/2024", "2025", "sem ano")), c(2024L, 2025L, NA_integer_))
  expect_equal(explicit_period_month(c("Jan/2024", "Fevereiro/2024", "2024")), c(202401L, 202402L, NA_integer_))
})

test_that("latest period does not depend on source row order", {
  periods <- data.frame(
    id = c("Jan/2024", "Dez/2025", "Mar/2024"),
    value = letters[1:3]
  )
  expect_equal(latest_period_row(periods)$id, "Dez/2025")
})

test_that("latest period can be inferred from the source value", {
  periods <- data.frame(
    id = c("Competência A", "Competência B"),
    value = c("arquivo_202401.dbf", "arquivo_202412.dbf")
  )
  expect_equal(latest_period_row(periods)$id, "Competência B")
})

test_that("monthly sources default to the latest completed lagged period", {
  periods <- data.frame(
    id = c("Jun/2026", "Mai/2026", "Abr/2026"),
    value = c("file2606.dbf", "file2605.dbf", "file2604.dbf")
  )
  expect_equal(default_period_row(periods, "monthly")$id, "Mai/2026")
  expect_equal(default_period_row(periods, "annual")$id, "Jun/2026")
})

test_that("query validates fields and monthly person-time", {
  query <- mock_query()
  expect_s3_class(query, "datasus_query")
  expect_equal(query$uf, "AC")
  weights <- query_period_weights(query)
  expect_equal(weights$year, 2024L)
  expect_equal(weights$weight, 2 / 12)
})

test_that("SIM state query rejects a simultaneous UF restriction", {
  options <- mock_options()
  expect_error(
    new_datasus_query(
      domain = "sim",
      dataset = "obitos",
      uf = "AC",
      geo_level = "state",
      measure = options$conteudo,
      periods = options$periodo,
      options = options
    ),
    "deixe a abrangência em Brasil"
  )
})
