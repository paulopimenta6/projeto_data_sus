load_microdata_sample <- function(name) {
  environment <- new.env(parent = emptyenv())
  utils::data(list = name, package = "microdatasus", envir = environment)
  get(name, envir = environment, inherits = FALSE)
}

new_microdata_sample_query <- function(
  domain,
  dataset,
  period,
  measure_value,
  uf = "AC",
  geo_level = "municipality",
  condition_field = NULL,
  condition_values = character(),
  urgent_only = FALSE
) {
  options <- fetch_microdata_options(domain, dataset, uf, geo_level)
  measure <- options$conteudo[options$conteudo$value == measure_value, , drop = FALSE]
  new_datasus_query(
    domain = domain,
    dataset = dataset,
    uf = uf,
    geo_level = geo_level,
    measure = measure,
    periods = period,
    options = options,
    condition_field = condition_field,
    condition_values = condition_values,
    urgent_only = urgent_only
  )
}

test_that("provider metadata is local, explicit, and bounded", {
  options <- fetch_datasus_options(
    "sih_morbidade", "geral_internacao", uf = "AC", geo_level = "municipality"
  )
  expect_s3_class(options, "microdata_options")
  expect_equal(attr(options, "provider"), "microdata")
  expect_equal(options$source$source, "SIHSUS")
  expect_equal(options$source$file_type, "RD")
  expect_equal(options$max_periods, 12L)
  expect_true(all(c("internacoes", "obitos_hospitalares", "valor_total") %in% options$conteudo$value))
  expect_true(all(c("diagnostico_cid10", "procedimento_sigtap") %in% names(options$filtros)))
  expect_equal(nrow(options$filtros$procedimento_sigtap), 0L)
  expect_equal(nrow(options$filtros$municipio_internacao), 0L)
  expect_equal(options$filter_choice_types[["procedimento_sigtap"]], "procedure")
  classified <- classify_source_filters(options)
  expect_equal(classified$role[classified$field == "carater_atendimento"], "urgency")
  expect_equal(classified$label[classified$field == "diagnostico_cid10"], "Capítulo do diagnóstico principal (CID-10)")
})

test_that("large filter choices are loaded lazily for server-side search", {
  options <- fetch_microdata_options(
    "sih_morbidade", "geral_internacao", uf = "AC", geo_level = "municipality"
  )
  procedures <- source_filter_choices(
    options, "sih_morbidade", "geral_internacao", "procedimento_sigtap", "AC"
  )
  municipalities <- source_filter_choices(
    options, "sih_morbidade", "geral_internacao", "municipio_internacao", "AC"
  )

  expect_gt(nrow(procedures), 1000L)
  expect_gt(nrow(municipalities), 0L)
  expect_true(all(startsWith(municipalities$value, "12")))
})

test_that("DBC readers receive only columns required by the query", {
  query <- new_microdata_sample_query(
    "sih_morbidade",
    "geral_internacao",
    data.frame(id = "Jun/2016", value = "201606"),
    "internacoes"
  )
  spec <- microdata_source_spec(query$domain, query$dataset)
  expect_setequal(
    microdata_query_columns(spec, query),
    c("MUNIC_MOV", "DIAG_PRINC", "ANO_CMPT", "MES_CMPT")
  )

  query$filters$procedimento_sigtap <- "0303010037"
  query$urgent_only <- TRUE
  expect_setequal(
    microdata_query_columns(spec, query),
    c("MUNIC_MOV", "DIAG_PRINC", "ANO_CMPT", "MES_CMPT", "PROC_REA", "CAR_INT")
  )
  expect_false("VAL_TOT" %in% microdata_query_columns(spec, query))

  query$scale <- "age_standardized_rate"
  expect_true(all(c("IDADE", "COD_IDADE") %in% microdata_query_columns(spec, query)))
})

test_that("national municipality filters process only selected states", {
  query <- new_microdata_sample_query(
    "sih_morbidade",
    "geral_internacao",
    data.frame(id = "Jun/2016", value = "201606"),
    "internacoes",
    uf = NULL
  )
  query$filters$municipio_internacao <- c("120040", "355030")
  spec <- microdata_source_spec(query$domain, query$dataset)

  expect_equal(microdata_query_ufs(query, spec), c("AC", "SP"))
})

test_that("SIH records aggregate without TABNET and preserve totals", {
  data <- standardize_microdata_columns(load_microdata_sample("sih_rd_sample"))
  query <- new_microdata_sample_query(
    "sih_morbidade",
    "geral_internacao",
    data.frame(id = "Jun/2016", value = "201606"),
    "internacoes"
  )
  bundle <- aggregate_microdata_bundle(data, query)

  expect_equal(bundle$total, nrow(data))
  expect_equal(sum(bundle$map$value), bundle$total)
  expect_equal(sum(bundle$series$value), bundle$total)
  expect_true(all(grepl("^[0-9]{7} ", bundle$map$label)))
  expect_true(any(grepl("CID-10|Doenças|Afecções|Causas", bundle$ranking$label)))
})

test_that("CID chapter and urgency filters are applied to SIH records", {
  data <- standardize_microdata_columns(load_microdata_sample("sih_rd_sample"))
  query <- new_microdata_sample_query(
    "sih_morbidade",
    "geral_internacao",
    data.frame(id = "Jun/2016", value = "201606"),
    "internacoes",
    condition_field = "diagnostico_cid10",
    condition_values = "A00-B99",
    urgent_only = TRUE
  )
  bundle <- aggregate_microdata_bundle(data, query)
  expected <- data$CAR_INT == "02" & match_icd_ranges(data$DIAG_PRINC, "A00-B99")
  expect_equal(bundle$total, sum(expected))

  query$filters$diagnostico_cid10 <- "A08"
  exact_bundle <- aggregate_microdata_bundle(data, query)
  exact_expected <- data$CAR_INT == "02" & startsWith(data$DIAG_PRINC, "A08")
  expect_equal(exact_bundle$total, sum(exact_expected))
})

test_that("SIA additive measures use approved quantities rather than row counts", {
  data <- standardize_microdata_columns(load_microdata_sample("sia_pa_sample"))
  query <- new_microdata_sample_query(
    "sia",
    "atendimento",
    data.frame(id = "Jun/2016", value = "201606"),
    "quantidade_aprovada"
  )
  bundle <- aggregate_microdata_bundle(data, query)
  expect_equal(bundle$total, sum(data$PA_QTDAPR))
  expect_equal(sum(bundle$ranking$value), bundle$total)
  expect_true(any(grepl(" - ", bundle$ranking$label, fixed = TRUE)))
})

test_that("CNES snapshots do not add establishments across months", {
  june <- standardize_microdata_columns(load_microdata_sample("cnes_st_sample"))
  may <- june
  may$COMPETEN <- "201605"
  data <- dplyr::bind_rows(may, june)
  query <- new_microdata_sample_query(
    "cnes",
    "estabelecimentos",
    data.frame(id = c("Mai/2016", "Jun/2016"), value = c("201605", "201606")),
    "estabelecimentos"
  )
  bundle <- aggregate_microdata_bundle(data, query)
  expect_equal(bundle$total, dplyr::n_distinct(june$CNES))
  expect_equal(nrow(bundle$series), 2L)
  expect_equal(bundle$series$value, rep(dplyr::n_distinct(june$CNES), 2L))
  expect_match(bundle$warnings, "última competência")
})

test_that("confirmed periods and territories with no events are completed with zero", {
  original_lookup <- microdata_municipality_lookup
  on.exit(assign("microdata_municipality_lookup", original_lookup, envir = globalenv()), add = TRUE)
  assign(
    "microdata_municipality_lookup",
    function(uf = NULL) {
      data.frame(
        code6 = c("120001", "120040"),
        code7 = c("1200013", "1200401"),
        label = c("Acrelândia", "Rio Branco")
      )
    },
    envir = globalenv()
  )

  data <- standardize_microdata_columns(load_microdata_sample("sih_rd_sample"))
  data <- data[substr(data$MUNIC_MOV, 1L, 6L) == "120040", , drop = FALSE]
  query <- new_microdata_sample_query(
    "sih_morbidade",
    "geral_internacao",
    data.frame(id = c("Jun/2016", "Jul/2016"), value = c("201606", "201607")),
    "internacoes"
  )
  bundle <- aggregate_microdata_bundle(data, query)

  expect_equal(bundle$series$value, c(nrow(data), 0))
  expect_equal(bundle$map$value[grepl("1200013", bundle$map$label)], 0)
})

test_that("SINAN national files can be restricted by residence UF", {
  data <- standardize_microdata_columns(load_microdata_sample("sinan_dengue_sample"))
  query <- new_microdata_sample_query(
    "sinan",
    "dengue",
    data.frame(id = "2010", value = "2010"),
    "notificacoes",
    uf = "RO"
  )
  bundle <- aggregate_microdata_bundle(data, query)
  expect_equal(bundle$total, sum(substr(data$ID_MN_RESI, 1L, 2L) == uf_code("RO")))
  expect_equal(sum(bundle$map$value), bundle$total)
})

test_that("SINASC proportions retain numerator, eligible records, and missingness", {
  data <- standardize_microdata_columns(load_microdata_sample("sinasc_sample"))
  query <- new_microdata_sample_query(
    "sinasc",
    "nascidos_vivos",
    data.frame(id = "2016", value = "2016"),
    "baixo_peso"
  )
  bundle <- aggregate_microdata_bundle(data, query)
  known <- is.finite(suppressWarnings(as.numeric(data$PESO)))
  expected <- sum(known & as.numeric(data$PESO) < 2500) / sum(known) * 100

  expect_equal(bundle$total, expected)
  expect_equal(bundle$quality$eligible_records, sum(known))
  expect_equal(bundle$quality$missing_measure_records, sum(!known))
  expect_equal(bundle$ranking_kind, "territory")
  expect_true(any(is.na(bundle$map$value)))
})

test_that("source registry declares measure semantics instead of reading labels", {
  sih <- microdata_source_spec("sih_morbidade", "geral_internacao")
  expect_true(all(c(
    "measure_type", "unit", "multiplier", "rate_eligible", "standardizable"
  ) %in% names(sih$measures)))
  expect_true(sih$measures$standardizable[sih$measures$value == "internacoes"])
  expect_false(sih$measures$rate_eligible[sih$measures$value == "valor_total"])
  expect_equal(sih$geography_semantics, "Município de internação/processamento")
})

test_that("expanded epidemiological sources are registered", {
  catalog <- load_datasus_catalog()
  expect_true(all(c(
    "chagas", "leishmaniose_tegumentar", "leishmaniose_visceral", "nascidos_vivos"
  ) %in% catalog$conjunto))
  expect_equal(microdata_source_spec("sinasc", "nascidos_vivos")$information_system, "SINASC")
})

test_that("geographic variants retain the source contract without duplicated fields", {
  sim <- microdata_source_spec("sim", "obitos_ocorrencia")
  sih <- microdata_source_spec("sih_morbidade", "internacoes_residencia")
  sinasc <- microdata_source_spec("sinasc", "nascidos_vivos_ocorrencia")

  expect_equal(sim$geo_column, "CODMUNOCOR")
  expect_equal(sim$geography_semantics, "Município de ocorrência do óbito")
  expect_equal(sih$geo_column, "MUNIC_RES")
  expect_equal(sih$geography_semantics, "Município de residência")
  expect_equal(sinasc$geo_column, "CODMUNNASC")
  expect_equal(sinasc$geography_semantics, "Município de ocorrência do nascimento")
  expect_true(all(vapply(list(sim, sih, sinasc), function(spec) {
    !anyDuplicated(names(spec))
  }, logical(1))))
})

test_that("source schemas omit fields unavailable in a specific disease", {
  malaria <- microdata_source_spec("sinan", "malaria")
  expect_false(any(c("CRITERIO", "EVOLUCAO") %in% microdata_required_columns(malaria)))
  leptospirosis <- microdata_source_spec("sinan", "leptospirose")
  expect_true(all(c("CRITERIO", "EVOLUCAO") %in% microdata_required_columns(leptospirosis)))
})

test_that("provider fallback is explicit and never returns a silent partial result", {
  original_primary <- fetch_with_datasusr
  original_fallback <- fetch_with_microdatasus
  on.exit(assign("fetch_with_datasusr", original_primary, envir = globalenv()), add = TRUE)
  on.exit(assign("fetch_with_microdatasus", original_fallback, envir = globalenv()), add = TRUE)
  assign("fetch_with_datasusr", function(...) stop("primary unavailable"), envir = globalenv())
  assign(
    "fetch_with_microdatasus",
    function(...) {
      result <- tibble::tibble(ANO_CMPT = 2024L, .SOURCE_FILE = "RDAC2401.dbc")
      attr(result, "source_manifest") <- tibble::tibble(
        file = "RDAC2401.dbc", url = NA_character_, release = "microdatasus",
        sha256 = NA_character_, retrieved_at = Sys.time()
      )
      result
    },
    envir = globalenv()
  )

  spec <- microdata_source_spec("sih_morbidade", "geral_internacao")
  parts <- data.frame(label = "Jan/2024", key = "202401", year = 2024L, month = 1L)
  result <- fetch_microdata_slice(spec, parts, uf = "AC")
  expect_equal(result$provider, "microdatasus")
  expect_match(result$warnings, "primary unavailable")

  assign("fetch_with_microdatasus", function(...) stop("fallback unavailable"), envir = globalenv())
  expect_error(fetch_microdata_slice(spec, parts, uf = "AC"), "Nenhum provedor")
})

test_that("microdatasus fallback requires every expected source file", {
  spec <- microdata_source_spec("sih_morbidade", "geral_internacao")
  parts <- data.frame(
    label = c("Jan/2024", "Fev/2024"),
    key = c("202401", "202402"),
    year = c(2024L, 2024L),
    month = c(1L, 2L)
  )
  partial_fetch <- function(...) {
    arguments <- list(...)
    if (arguments$month_start == 2L) return(NULL)
    tibble::tibble(
      ANO_CMPT = 2024L,
      MES_CMPT = 1L,
      source = "RDAC2401.dbc"
    )
  }

  expect_error(
    fetch_with_microdatasus(spec, parts, uf = "AC", fetch_function = partial_fetch),
    "não retornou o arquivo esperado.*Fev/2024"
  )
})

test_that("DBC cache identity includes URL and stale files are refreshed", {
  calls <- 0L
  fake_download <- function(
    files,
    use_cache,
    dest_dir,
    overwrite,
    timeout,
    verbose
  ) {
    calls <<- calls + 1L
    directory <- file.path(dest_dir, files$source[[1L]], files$file_type[[1L]])
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    local_file <- file.path(directory, files$file_name[[1L]])
    writeBin(charToRaw(files$url[[1L]]), local_file)
    dplyr::mutate(files, local_file = local_file)
  }
  file <- tibble::tibble(
    source = "SIHSUS",
    file_type = "RD",
    period = "current",
    file_name = "RDAC2401.dbc",
    url = "ftp://example.test/current/RDAC2401.dbc"
  )

  first <- download_microdata_dbc(file, download_function = fake_download)
  cached <- download_microdata_dbc(file, download_function = fake_download)
  revised <- file
  revised$url <- "ftp://example.test/prelim/RDAC2401.dbc"
  second_url <- download_microdata_dbc(revised, download_function = fake_download)

  expect_equal(calls, 2L)
  expect_equal(first$local_file, cached$local_file)
  expect_false(identical(first$local_file, second_url$local_file))
  expect_false(identical(first$checksum_sha256, second_url$checksum_sha256))

  size <- file.info(first$local_file)$size
  writeBin(rep(as.raw(0), size), first$local_file)
  repaired <- download_microdata_dbc(file, download_function = fake_download)
  expect_equal(calls, 3L)
  expect_equal(repaired$checksum_sha256, first$checksum_sha256)

  Sys.setFileTime(first$local_file, Sys.time() - MICRODATA_DBC_CACHE_MAX_AGE - 1)
  refreshed <- download_microdata_dbc(file, download_function = fake_download)
  expect_equal(calls, 4L)
  expect_true(refreshed$downloaded)
})

test_that("national microdata queries enforce source-specific period limits", {
  options <- fetch_microdata_options(
    "sih_morbidade", "geral_internacao", uf = NULL, geo_level = "state"
  )
  expect_equal(options$max_periods, 1L)
  expect_error(
    new_datasus_query(
      domain = "sih_morbidade",
      dataset = "geral_internacao",
      uf = NULL,
      geo_level = "state",
      measure = options$conteudo[1L, , drop = FALSE],
      periods = data.frame(
        id = c("Jan/2024", "Fev/2024"),
        value = c("202401", "202402")
      ),
      options = options
    ),
    "no máximo 1 período"
  )
})
