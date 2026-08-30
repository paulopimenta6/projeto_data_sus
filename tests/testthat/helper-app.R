source(testthat::test_path("..", "..", "global.R"), chdir = TRUE)

Sys.setenv(PROJETO_DATASUS_CACHE_DIR = tempfile("projeto-datasus-test-cache-"))

mock_options <- function() {
  list(
    linha = data.frame(
      id = c("Unidade da Federação", "Município", "Região de Saúde (CIR)", "Capítulo CID-10", "Ano"),
      value = c("uf", "municipio", "regiao", "cid", "ano"),
      stringsAsFactors = FALSE
    ),
    coluna = data.frame(id = "Não ativa", value = "-", stringsAsFactors = FALSE),
    conteudo = data.frame(id = "Internações", value = "internacoes", stringsAsFactors = FALSE),
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
