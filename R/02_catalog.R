DOMAIN_CONFIG <- list(
  sim = list(
    label = "Mortalidade (SIM)",
    system = "sim",
    provider = "microdata",
    source_function = "sim",
    default_dataset = "obitos",
    frequency = "annual",
    ranking_terms = c("capitulo cid", "categoria cid", "causa", "cid 10")
  ),
  sih_morbidade = list(
    label = "Morbidade hospitalar (SIH)",
    system = "sih",
    provider = "microdata",
    source_function = "sih_morbidade",
    default_dataset = "geral_internacao",
    frequency = "monthly",
    ranking_terms = c("capitulo cid", "categoria cid", "diagnostico", "cid 10")
  ),
  sih_producao = list(
    label = "Produção hospitalar (SIH)",
    system = "sih",
    provider = "microdata",
    source_function = "sih_producao",
    default_dataset = "aih_rd_internacao",
    frequency = "monthly",
    ranking_terms = c("procedimento", "grupo procedimento", "especialidade")
  ),
  sia = list(
    label = "Produção ambulatorial e urgência (SIA)",
    system = "sia",
    provider = "microdata",
    source_function = "sia_producao",
    default_dataset = "atendimento",
    frequency = "monthly",
    ranking_terms = c("procedimento", "grupo procedimento", "cid", "tipo unidade")
  ),
  cnes = list(
    label = "Estabelecimentos e capacidade (CNES)",
    system = "cnes",
    provider = "microdata",
    source_function = "cnes",
    default_dataset = "estabelecimentos",
    frequency = "snapshot",
    ranking_terms = c("tipo estabelecimento", "tipo unidade", "natureza", "esfera")
  ),
  sinan = list(
    label = "Agravos de notificação (SINAN)",
    system = "sinan",
    provider = "microdata",
    source_function = "sinan",
    default_dataset = "dengue",
    frequency = "annual",
    ranking_terms = c("classificacao final", "criterio confirmacao", "evolucao", "faixa etaria")
  )
)

MICRODATA_DATASET_CONFIG <- list(
  "sim/obitos" = list(
    domain = "sim", system = "sim", dataset = "obitos",
    source = "SIM", file_type = "DO", information_system = "SIM-DO",
    category = "mortalidade", description = "Óbitos por residência"
  ),
  "sih_morbidade/geral_internacao" = list(
    domain = "sih_morbidade", system = "sih", dataset = "geral_internacao",
    source = "SIHSUS", file_type = "RD", information_system = "SIH-RD",
    category = "morbidade hospitalar", description = "Morbidade hospitalar geral"
  ),
  "sih_producao/aih_rd_internacao" = list(
    domain = "sih_producao", system = "sih", dataset = "aih_rd_internacao",
    source = "SIHSUS", file_type = "RD", information_system = "SIH-RD",
    category = "produção hospitalar", description = "Produção de internações hospitalares"
  ),
  "sia/atendimento" = list(
    domain = "sia", system = "sia", dataset = "atendimento",
    source = "SIASUS", file_type = "PA", information_system = "SIA-PA",
    category = "produção ambulatorial", description = "Produção ambulatorial"
  ),
  "cnes/estabelecimentos" = list(
    domain = "cnes", system = "cnes", dataset = "estabelecimentos",
    source = "CNES", file_type = "ST", information_system = "CNES-ST",
    category = "estabelecimentos", description = "Estabelecimentos de saúde"
  ),
  "cnes/leitos_internacao" = list(
    domain = "cnes", system = "cnes", dataset = "leitos_internacao",
    source = "CNES", file_type = "LT", information_system = "CNES-LT",
    category = "recursos físicos", description = "Leitos de internação"
  ),
  "sinan/dengue" = list(
    domain = "sinan", system = "sinan", dataset = "dengue",
    source = "SINAN", file_type = "DENG", information_system = "SINAN-DENGUE",
    category = "agravos de notificação", description = "Dengue"
  ),
  "sinan/chikungunya" = list(
    domain = "sinan", system = "sinan", dataset = "chikungunya",
    source = "SINAN", file_type = "CHIK", information_system = "SINAN-CHIKUNGUNYA",
    category = "agravos de notificação", description = "Chikungunya"
  ),
  "sinan/zika" = list(
    domain = "sinan", system = "sinan", dataset = "zika",
    source = "SINAN", file_type = "ZIKA", information_system = "SINAN-ZIKA",
    category = "agravos de notificação", description = "Zika"
  ),
  "sinan/malaria" = list(
    domain = "sinan", system = "sinan", dataset = "malaria",
    source = "SINAN", file_type = "MALA", information_system = "SINAN-MALARIA",
    category = "agravos de notificação", description = "Malária"
  ),
  "sinan/leptospirose" = list(
    domain = "sinan", system = "sinan", dataset = "leptospirose",
    source = "SINAN", file_type = "LEPT", information_system = "SINAN-LEPTOSPIROSE",
    category = "agravos de notificação", description = "Leptospirose"
  )
)

microdata_dataset_key <- function(domain, dataset) {
  paste(domain, dataset, sep = "/")
}

is_microdata_dataset <- function(domain, dataset) {
  microdata_dataset_key(domain, dataset) %in% names(MICRODATA_DATASET_CONFIG)
}

get_microdata_dataset_config <- function(domain, dataset) {
  config <- MICRODATA_DATASET_CONFIG[[microdata_dataset_key(domain, dataset)]]
  if (is.null(config)) {
    stop(
      "O conjunto selecionado ainda não possui adaptador de microdados: ",
      domain, "/", dataset, ".",
      call. = FALSE
    )
  }
  config
}

microdata_catalog <- function() {
  rows <- lapply(MICRODATA_DATASET_CONFIG, function(config) {
    data.frame(
      domain = config$domain,
      sistema = config$system,
      conjunto = config$dataset,
      categoria = config$category,
      descricao = config$description,
      escopo = "all",
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

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
  microdata_catalog()
}

catalog_for_domain <- function(domain, catalog = load_datasus_catalog()) {
  config <- get_domain_config(domain)
  if ("domain" %in% names(catalog)) {
    result <- catalog[catalog$domain == domain, , drop = FALSE]
  } else {
    result <- catalog[catalog$sistema == config$system, , drop = FALSE]
  }

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
  configured_roles <- options$filter_roles %||% character()
  role <- unname(configured_roles[fields])
  role[is.na(role) | !nzchar(role)] <- "other"
  inferred <- !fields %in% names(configured_roles)
  role[inferred & grepl("municip|regiao.*saude|macrorreg|unidade.*feder|(^|_)uf($|_)", normalized)] <- "territory"
  role[inferred & grepl("cid|causa|diagn|proced|agravo|doenca|morbidade", normalized)] <- "condition"
  role[inferred & grepl("carater.*atend|urgenc|emergenc", normalized)] <- "urgency"
  configured_labels <- options$filter_labels %||% character()
  labels <- unname(configured_labels[fields])
  missing_labels <- is.na(labels) | !nzchar(labels)
  labels[missing_labels] <- vapply(fields[missing_labels], pretty_filter_name, character(1))

  data.frame(
    field = fields,
    label = labels,
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
