DOMAIN_CONFIG <- list(
  sim = list(
    label = "Mortalidade (SIM)",
    system = "sim",
    source_function = "sim",
    default_dataset = "obitos",
    frequency = "annual",
    ranking_terms = c("capitulo cid", "categoria cid", "causa", "cid 10")
  ),
  sih_morbidade = list(
    label = "Morbidade hospitalar (SIH)",
    system = "sih",
    source_function = "sih_morbidade",
    default_dataset = "geral_internacao",
    frequency = "monthly",
    ranking_terms = c("capitulo cid", "categoria cid", "diagnostico", "cid 10")
  ),
  sih_producao = list(
    label = "Produção hospitalar (SIH)",
    system = "sih",
    source_function = "sih_producao",
    default_dataset = "aih_rd_internacao",
    frequency = "monthly",
    ranking_terms = c("procedimento", "grupo procedimento", "especialidade")
  ),
  sia = list(
    label = "Produção ambulatorial e urgência (SIA)",
    system = "sia",
    source_function = "sia_producao",
    default_dataset = "atendimento",
    frequency = "monthly",
    ranking_terms = c("procedimento", "grupo procedimento", "cid", "tipo unidade")
  ),
  cnes = list(
    label = "Estabelecimentos e capacidade (CNES)",
    system = "cnes",
    source_function = "cnes",
    default_dataset = "estabelecimentos",
    frequency = "snapshot",
    ranking_terms = c("tipo estabelecimento", "tipo unidade", "natureza", "esfera")
  ),
  sinan = list(
    label = "Agravos de notificação (SINAN)",
    system = "sinan",
    source_function = "sinan",
    default_dataset = "dengue",
    frequency = "annual",
    ranking_terms = c("classificacao final", "criterio confirmacao", "evolucao", "faixa etaria")
  )
)

fallback_catalog <- function() {
  data.frame(
    sistema = c(
      "sim", "sih", "sih", "sia", "cnes", "cnes", "sinan", "sinan",
      "sinan", "sinan", "sinan"
    ),
    conjunto = c(
      "obitos", "geral_internacao", "aih_rd_internacao", "atendimento",
      "estabelecimentos", "leitos_internacao", "dengue", "chikungunya",
      "zika", "malaria", "leptospirose"
    ),
    categoria = c(
      "mortalidade", "morbidade hospitalar", "produção hospitalar",
      "produção ambulatorial", "estabelecimentos", "recursos físicos",
      rep("agravos de notificação", 5)
    ),
    descricao = c(
      "Óbitos por residência ou ocorrência",
      "Morbidade hospitalar geral",
      "Produção de internações hospitalares",
      "Produção ambulatorial",
      "Estabelecimentos de saúde",
      "Leitos de internação",
      "Dengue", "Chikungunya", "Zika", "Malária", "Leptospirose"
    ),
    escopo = "all",
    stringsAsFactors = FALSE
  )
}

domain_choices <- function() {
  stats::setNames(
    names(DOMAIN_CONFIG),
    vapply(DOMAIN_CONFIG, `[[`, character(1), "label")
  )
}

get_domain_config <- function(domain) {
  config <- DOMAIN_CONFIG[[domain]]
  if (is.null(config)) {
    stop("Domínio de análise desconhecido: ", domain, call. = FALSE)
  }
  config
}

load_datasus_catalog <- function() {
  catalog <- tryCatch(
    datasus::datasus_catalogo(),
    error = function(error) NULL
  )
  if (is.null(catalog) || !all(c("sistema", "conjunto", "descricao") %in% names(catalog))) {
    return(fallback_catalog())
  }
  catalog
}

catalog_for_domain <- function(domain, catalog = load_datasus_catalog()) {
  config <- get_domain_config(domain)
  result <- catalog[catalog$sistema == config$system, , drop = FALSE]

  if (domain == "sih_morbidade" && "categoria" %in% names(result)) {
    keep <- grepl("morbidade", normalize_text(result$categoria), fixed = TRUE)
    if (any(keep)) result <- result[keep, , drop = FALSE]
  }

  if (domain == "sih_producao" && "categoria" %in% names(result)) {
    keep <- !grepl("morbidade", normalize_text(result$categoria), fixed = TRUE)
    if (any(keep)) result <- result[keep, , drop = FALSE]
  }

  if (nrow(result) == 0L) {
    result <- fallback_catalog()
    result <- result[result$sistema == config$system, , drop = FALSE]
  }

  result$label <- paste0(result$descricao, " [", result$conjunto, "]")
  result <- result[!duplicated(result$conjunto), , drop = FALSE]
  result[order(result$descricao), , drop = FALSE]
}

dataset_choices <- function(domain, catalog = load_datasus_catalog()) {
  datasets <- catalog_for_domain(domain, catalog)
  stats::setNames(datasets$conjunto, datasets$label)
}

pretty_filter_name <- function(field) {
  label <- gsub("_", " ", field, fixed = TRUE)
  tools::toTitleCase(label)
}

classify_source_filters <- function(options) {
  filters <- options$filtros %||% list()
  if (length(filters) == 0L) {
    return(data.frame(
      field = character(), label = character(), role = character(),
      stringsAsFactors = FALSE
    ))
  }

  fields <- names(filters)
  normalized <- normalize_text(fields)
  role <- rep("other", length(fields))
  role[grepl("municip|regiao.*saude|macrorreg|unidade.*feder|(^|_)uf($|_)", normalized)] <- "territory"
  role[grepl("cid|causa|diagn|proced|agravo|doenca|morbidade", normalized)] <- "condition"
  role[grepl("carater.*atend|urgenc|emergenc", normalized)] <- "urgency"

  data.frame(
    field = fields,
    label = vapply(fields, pretty_filter_name, character(1)),
    role = role,
    stringsAsFactors = FALSE
  )
}

score_option <- function(labels, terms) {
  normalized_labels <- normalize_text(labels)
  normalized_terms <- normalize_text(terms)
  scores <- rep(0, length(labels))
  for (index in seq_along(normalized_terms)) {
    term <- normalized_terms[[index]]
    exact <- normalized_labels == term
    contains <- grepl(term, normalized_labels, fixed = TRUE)
    reverse_contains <- vapply(
      normalized_labels,
      function(label) nzchar(label) && grepl(label, term, fixed = TRUE),
      logical(1)
    )
    scores <- pmax(scores, ifelse(exact, 100 - index, 0))
    scores <- pmax(scores, ifelse(contains, 70 - index, 0))
    scores <- pmax(scores, ifelse(reverse_contains, 40 - index, 0))
  }
  scores
}

resolve_option <- function(option_table, terms, exclude_values = character()) {
  if (is.null(option_table) || nrow(option_table) == 0L) return(NULL)
  eligible <- !(as.character(option_table$value) %in% exclude_values)
  scores <- score_option(option_table$id, terms)
  scores[!eligible] <- -Inf
  if (!any(is.finite(scores) & scores > 0)) return(NULL)
  option_table[which.max(scores), , drop = FALSE]
}

resolve_geography_dimension <- function(options, geo_level) {
  terms <- switch(
    geo_level,
    state = c("Unidade da Federação", "UF", "Estado"),
    municipality = c("Município de residência", "Município de ocorrência", "Município"),
    health_region = c("Região de Saúde", "Região saúde", "Regional de Saúde"),
    stop("Nível geográfico desconhecido.", call. = FALSE)
  )
  resolve_option(options$linha, terms)
}

resolve_time_dimension <- function(options, frequency = "monthly") {
  terms <- if (frequency == "annual") {
    c("Ano do Óbito", "Ano de notificação", "Ano", "Período")
  } else {
    c("Mês/Ano", "Ano/Mês", "Competência", "Ano de competência", "Período")
  }
  resolve_option(options$linha, terms)
}

resolve_ranking_dimension <- function(options, domain, exclude_values = character()) {
  config <- get_domain_config(domain)
  resolve_option(options$linha, config$ranking_terms, exclude_values)
}

resolve_inactive_column <- function(options) {
  inactive <- resolve_option(options$coluna, c("Não ativa", "Não ativo", "Nenhuma"))
  if (!is.null(inactive)) return(inactive)
  if (is.null(options$coluna) || nrow(options$coluna) == 0L) return(NULL)
  options$coluna[1L, , drop = FALSE]
}
