source(testthat::test_path("..", "..", "global.R"), chdir = TRUE)

Sys.setenv(PROJETO_DATASUS_CACHE_DIR = tempfile("projeto-datasus-test-cache-"))

test_municipalities <- load_microdatasus_dataset("tabMun")
test_code6 <- format_integer_code(test_municipalities$munResCod, 6L)
test_keep <- !is.na(test_code6) & test_municipalities$munResTipo == "MUNIC" &
  test_municipalities$munResStatus == "ATIVO"
test_check_digit <- vapply(strsplit(test_code6[test_keep], "", fixed = TRUE), function(digits) {
  products <- as.integer(digits) * c(1L, 2L, 1L, 2L, 1L, 2L)
  sum_digits <- sum((products %/% 10L) + (products %% 10L))
  as.character((10L - sum_digits %% 10L) %% 10L)
}, character(1))
options(projeto_datasus.municipality_crosswalk = data.frame(
  code6 = test_code6[test_keep],
  code7 = paste0(test_code6[test_keep], test_check_digit),
  name = as.character(test_municipalities$munResNome[test_keep]),
  stringsAsFactors = FALSE
))

mock_options <- function() {
  list(
    linha = data.frame(
      id = c("Unidade da Federação", "Município", "Região de Saúde (CIR)", "Capítulo CID-10", "Ano"),
      value = c("uf", "municipio", "regiao", "cid", "ano"),
      stringsAsFactors = FALSE
    ),
    coluna = data.frame(id = "Não ativa", value = "-", stringsAsFactors = FALSE),
    conteudo = data.frame(
      id = "Internações", value = "internacoes", measure_type = "count",
      unit = "AIH", multiplier = 100000, rate_eligible = TRUE,
      standardizable = TRUE, stringsAsFactors = FALSE
    ),
    periodo = data.frame(
      id = c("Jan/2024", "Fev/2024"),
      value = c("file2401.dbf", "file2402.dbf"),
      stringsAsFactors = FALSE
    ),
    filtros = list(
      capitulo_cid_10 = data.frame(
        id = c("Todas as categorias", "I. Infecciosas"),
        value = c("all", "I"),
        stringsAsFactors = FALSE
      ),
      municipio = data.frame(
        id = c("Todos", "120040 Rio Branco"),
        value = c("all", "120040"),
        stringsAsFactors = FALSE
      ),
      carater_atendiment = data.frame(
        id = c("Todos", "Urgência", "Eletivo"),
        value = c("all", "02", "01"),
        stringsAsFactors = FALSE
      )
    )
  )
}

mock_query <- function(scale = "count") {
  options <- mock_options()
  new_datasus_query(
    domain = "sih_morbidade",
    dataset = "geral_internacao",
    uf = "AC",
    geo_level = "municipality",
    measure = options$conteudo,
    periods = options$periodo,
    options = options,
    scale = scale
  )
}
