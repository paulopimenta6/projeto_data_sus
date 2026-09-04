ICD10_CHAPTERS <- data.frame(
  id = c(
    "I. Algumas doenças infecciosas e parasitárias (A00-B99)",
    "II. Neoplasias (C00-D48)",
    "III. Doenças do sangue e transtornos imunitários (D50-D89)",
    "IV. Doenças endócrinas, nutricionais e metabólicas (E00-E90)",
    "V. Transtornos mentais e comportamentais (F00-F99)",
    "VI. Doenças do sistema nervoso (G00-G99)",
    "VII. Doenças do olho e anexos (H00-H59)",
    "VIII. Doenças do ouvido e da apófise mastoide (H60-H95)",
    "IX. Doenças do aparelho circulatório (I00-I99)",
    "X. Doenças do aparelho respiratório (J00-J99)",
    "XI. Doenças do aparelho digestivo (K00-K93)",
    "XII. Doenças da pele e do tecido subcutâneo (L00-L99)",
    "XIII. Doenças do sistema osteomuscular (M00-M99)",
    "XIV. Doenças do aparelho geniturinário (N00-N99)",
    "XV. Gravidez, parto e puerpério (O00-O99)",
    "XVI. Afecções originadas no período perinatal (P00-P96)",
    "XVII. Malformações congênitas (Q00-Q99)",
    "XVIII. Sintomas e achados anormais (R00-R99)",
    "XIX. Lesões e outras consequências de causas externas (S00-T98)",
    "XX. Causas externas de morbidade e mortalidade (V01-Y98)",
    "XXI. Fatores que influenciam o estado de saúde (Z00-Z99)",
    "XXII. Códigos para propósitos especiais (U00-U99)"
  ),
  value = c(
    "A00-B99", "C00-D48", "D50-D89", "E00-E90", "F00-F99", "G00-G99",
    "H00-H59", "H60-H95", "I00-I99", "J00-J99", "K00-K93", "L00-L99",
    "M00-M99", "N00-N99", "O00-O99", "P00-P96", "Q00-Q99", "R00-R99",
    "S00-T98", "V01-Y98", "Z00-Z99", "U00-U99"
  ),
  stringsAsFactors = FALSE
)

MICRODATA_LOOKUPS <- new.env(parent = emptyenv())
MICRODATA_DBC_CACHE_MAX_AGE <- 24 * 60 * 60

microdata_measure_table <- function(
  id,
  value,
  column = NA_character_,
  reducer = "sum",
  measure_type = "count",
  unit = "registros",
  multiplier = 100000,
  rate_eligible = TRUE,
  standardizable = FALSE,
  parameter = NA_real_
) {
  data.frame(
    id = id,
    value = value,
    column = column,
    reducer = reducer,
    measure_type = measure_type,
    unit = unit,
    multiplier = multiplier,
    rate_eligible = rate_eligible,
    standardizable = standardizable,
    parameter = parameter,
    stringsAsFactors = FALSE
  )
}

fixed_filter <- function(label, role, column, choices, matcher = "exact", urgent_values = character()) {
  list(
    label = label,
    role = role,
    column = column,
    choice_type = "fixed",
    choices = choices,
    matcher = matcher,
    urgent_values = urgent_values
  )
}

lookup_filter <- function(label, role, column, choice_type, matcher = "exact") {
  list(
    label = label,
    role = role,
    column = column,
    choice_type = choice_type,
    matcher = matcher,
    urgent_values = character()
  )
}

range_filter <- function(label, column, choices) {
  fixed_filter(label, "condition", column, choices, matcher = "numeric_range")
}

race_color_choices <- function(include_ignored = FALSE) {
  result <- data.frame(
    id = c("Branca", "Preta", "Amarela", "Parda", "Indígena"),
    value = as.character(1:5),
    stringsAsFactors = FALSE
  )
  if (isTRUE(include_ignored)) {
    result <- rbind(result, data.frame(id = "Ignorada", value = "9"))
  }
  result
}

microdata_source_spec <- function(domain, dataset) {
  source_config <- get_microdata_dataset_config(domain, dataset)
  key <- microdata_dataset_key(domain, dataset)
  urgency_choices <- data.frame(
    id = c("Eletivo", "Urgência"),
    value = c("01", "02"),
    stringsAsFactors = FALSE
  )
  notification_choices <- switch(
    dataset,
    dengue = data.frame(
      id = c(
        "Dengue clássico (legado)", "Dengue com complicações (legado)",
        "Febre hemorrágica do dengue (legado)", "Síndrome do choque do dengue (legado)",
        "Descartado", "Dengue", "Dengue com sinais de alarme", "Dengue grave"
      ),
      value = c("1", "2", "3", "4", "5", "10", "11", "12"),
      stringsAsFactors = FALSE
    ),
    chikungunya = data.frame(
      id = c("Descartado", "Chikungunya"), value = c("5", "13"), stringsAsFactors = FALSE
    ),
    zika = data.frame(
      id = c("Confirmado", "Descartado", "Inconclusivo"),
      value = c("1", "2", "8"),
      stringsAsFactors = FALSE
    ),
    malaria = data.frame(
      id = c("Confirmado", "Descartado"), value = c("1", "2"), stringsAsFactors = FALSE
    ),
    leptospirose = data.frame(
      id = c("Confirmado", "Descartado"), value = c("1", "2"), stringsAsFactors = FALSE
    ),
    data.frame(
      id = c("Confirmado", "Descartado"), value = c("1", "2"), stringsAsFactors = FALSE
    )
  )
  criterion_choices <- data.frame(
    id = c("Laboratorial", "Clínico-epidemiológico"),
    value = c("1", "2"),
    stringsAsFactors = FALSE
  )
  evolution_choices <- data.frame(
    id = c("Cura", "Óbito pelo agravo", "Óbito por outra causa", "Ignorado"),
    value = c("1", "2", "3", "9"),
    stringsAsFactors = FALSE
  )

  spec <- switch(
    key,
    "sim/obitos" = list(
      start_year = 1996L,
      geo_column = "CODMUNRES",
      geography_semantics = "Município de residência",
      period_kind = "year",
      period_column = "DTOBITO",
      age_column = "IDADE",
      age_coding = "datasus_encoded",
      measures = microdata_measure_table(
        "Óbitos", "obitos", reducer = "rows", unit = "óbitos", standardizable = TRUE
      ),
      ranking = list(column = "CAUSABAS", label = "Causa básica CID-10", lookup = "icd"),
      filters = list(
        causa_cid10 = lookup_filter("Capítulo da causa básica (CID-10)", "condition", "CAUSABAS", "icd", "icd"),
        sexo = fixed_filter(
          "Sexo", "condition", "SEXO",
          data.frame(id = c("Masculino", "Feminino"), value = c("1", "2"))
        ),
        raca_cor = fixed_filter("Raça/cor", "condition", "RACACOR", race_color_choices()),
        ocupacao = lookup_filter("Ocupação", "condition", "OCUP", "occupation"),
        municipio_residencia = lookup_filter("Município de residência", "territory", "CODMUNRES", "municipality")
      ),
      max_periods_uf = 10L,
      max_periods_national = 3L,
      uf_files = TRUE
    ),
    "sim/obitos_ocorrencia" = {
      base <- unclass(microdata_source_spec("sim", "obitos"))
      base$geo_column <- "CODMUNOCOR"
      base$geography_semantics <- "Município de ocorrência do óbito"
      base$filters$municipio_residencia <- NULL
      base$filters$municipio_ocorrencia <- lookup_filter(
        "Município de ocorrência", "territory", "CODMUNOCOR", "municipality"
      )
      base
    },
    "sih_morbidade/geral_internacao" = list(
      start_year = 2008L,
      geo_column = "MUNIC_MOV",
      geography_semantics = "Município de internação/processamento",
      period_kind = "year_month",
      period_columns = c("ANO_CMPT", "MES_CMPT"),
      age_column = "IDADE",
      age_coding = "sih_unit",
      age_unit_column = "COD_IDADE",
      measures = microdata_measure_table(
        c("Internações (AIH)", "Óbitos hospitalares", "Dias de permanência", "Valor total (R$)"),
        c("internacoes", "obitos_hospitalares", "dias_permanencia", "valor_total"),
        c(NA, "MORTE", "DIAS_PERM", "VAL_TOT"),
        c("rows", "sum", "sum", "sum"),
        c("count", "count", "duration", "currency"),
        c("AIH", "óbitos hospitalares", "dias", "reais"),
        100000,
        c(TRUE, TRUE, FALSE, FALSE),
        c(TRUE, TRUE, FALSE, FALSE)
      ),
      ranking = list(column = "DIAG_PRINC", label = "Diagnóstico principal CID-10", lookup = "icd"),
      filters = list(
        diagnostico_cid10 = lookup_filter(
          "Capítulo do diagnóstico principal (CID-10)", "condition", "DIAG_PRINC", "icd", "icd"
        ),
        procedimento_sigtap = lookup_filter("Procedimento realizado", "condition", "PROC_REA", "procedure"),
        sexo = fixed_filter(
          "Sexo", "condition", "SEXO",
          data.frame(id = c("Masculino", "Feminino"), value = c("1", "3"))
        ),
        raca_cor = fixed_filter("Raça/cor", "condition", "RACA_COR", race_color_choices()),
        municipio_internacao = lookup_filter("Município da internação", "territory", "MUNIC_MOV", "municipality"),
        carater_atendimento = fixed_filter(
          "Caráter do atendimento", "urgency", "CAR_INT", urgency_choices,
          urgent_values = "02"
        )
      ),
      max_periods_uf = 12L,
      max_periods_national = 1L,
      uf_files = TRUE
    ),
    "sih_morbidade/internacoes_residencia" = {
      base <- unclass(microdata_source_spec("sih_morbidade", "geral_internacao"))
      base$geo_column <- "MUNIC_RES"
      base$geography_semantics <- "Município de residência"
      base$filters$municipio_internacao <- NULL
      base$filters$municipio_residencia <- lookup_filter(
        "Município de residência", "territory", "MUNIC_RES", "municipality"
      )
      base
    },
    "sih_producao/aih_rd_internacao" = list(
      start_year = 2008L,
      geo_column = "MUNIC_MOV",
      geography_semantics = "Município de internação/processamento",
      period_kind = "year_month",
      period_columns = c("ANO_CMPT", "MES_CMPT"),
      age_column = "IDADE",
      age_coding = "sih_unit",
      age_unit_column = "COD_IDADE",
      measures = microdata_measure_table(
        c("AIH processadas", "Valor total (R$)", "Dias de permanência"),
        c("aih_processadas", "valor_total", "dias_permanencia"),
        c(NA, "VAL_TOT", "DIAS_PERM"),
        c("rows", "sum", "sum"),
        c("count", "currency", "duration"),
        c("AIH", "reais", "dias"),
        100000,
        c(TRUE, FALSE, FALSE),
        c(TRUE, FALSE, FALSE)
      ),
      ranking = list(column = "PROC_REA", label = "Procedimento realizado", lookup = "procedure"),
      filters = list(
        procedimento_sigtap = lookup_filter("Procedimento realizado", "condition", "PROC_REA", "procedure"),
        ocupacao_cbo = lookup_filter("Ocupação responsável (CBO)", "condition", "CBOR", "occupation"),
        diagnostico_cid10 = lookup_filter(
          "Capítulo do diagnóstico principal (CID-10)", "condition", "DIAG_PRINC", "icd", "icd"
        ),
        sexo = fixed_filter(
          "Sexo", "condition", "SEXO",
          data.frame(id = c("Masculino", "Feminino"), value = c("1", "3"))
        ),
        raca_cor = fixed_filter("Raça/cor", "condition", "RACA_COR", race_color_choices()),
        municipio_internacao = lookup_filter("Município da internação", "territory", "MUNIC_MOV", "municipality"),
        carater_atendimento = fixed_filter(
          "Caráter do atendimento", "urgency", "CAR_INT", urgency_choices,
          urgent_values = "02"
        )
      ),
      max_periods_uf = 12L,
      max_periods_national = 1L,
      uf_files = TRUE
    ),
    "sia/atendimento" = list(
      start_year = 2008L,
      geo_column = "PA_UFMUN",
      geography_semantics = "Município do estabelecimento",
      period_kind = "yyyymm",
      period_column = "PA_CMP",
      measures = microdata_measure_table(
        c(
          "Quantidade aprovada", "Quantidade apresentada", "Valor aprovado (R$)",
          "Valor apresentado (R$)", "Registros de produção"
        ),
        c("quantidade_aprovada", "quantidade_apresentada", "valor_aprovado", "valor_apresentado", "registros"),
        c("PA_QTDAPR", "PA_QTDPRO", "PA_VALAPR", "PA_VALPRO", NA),
        c("sum", "sum", "sum", "sum", "rows"),
        c("amount", "amount", "currency", "currency", "count"),
        c("procedimentos", "procedimentos", "reais", "reais", "registros"),
        100000,
        c(TRUE, TRUE, FALSE, FALSE, TRUE),
        FALSE
      ),
      ranking = list(column = "PA_PROC_ID", label = "Procedimento ambulatorial", lookup = "procedure"),
      filters = list(
        procedimento_sigtap = lookup_filter("Procedimento ambulatorial", "condition", "PA_PROC_ID", "procedure"),
        ocupacao_cbo = lookup_filter("Ocupação responsável (CBO)", "condition", "PA_CBOCOD", "occupation"),
        diagnostico_cid10 = lookup_filter(
          "Capítulo do diagnóstico principal (CID-10)", "condition", "PA_CIDPRI", "icd", "icd"
        ),
        municipio_atendimento = lookup_filter("Município do estabelecimento", "territory", "PA_UFMUN", "municipality"),
        carater_atendimento = fixed_filter(
          "Caráter do atendimento", "urgency", "PA_CATEND", urgency_choices,
          urgent_values = "02"
        )
      ),
      max_periods_uf = 3L,
      max_periods_national = 1L,
      uf_files = TRUE
    ),
    "cnes/estabelecimentos" = list(
      start_year = 2008L,
      geo_column = "CODUFMUN",
      geography_semantics = "Município do estabelecimento",
      period_kind = "yyyymm",
      period_column = "COMPETEN",
      measures = microdata_measure_table(
        "Estabelecimentos cadastrados", "estabelecimentos", "CNES", "distinct",
        "stock", "estabelecimentos", 100000, FALSE, FALSE
      ),
      ranking = list(column = "TP_UNID", label = "Tipo de estabelecimento", lookup = "cnes_unit"),
      filters = list(
        grupo_estabelecimento = fixed_filter(
          "Grupo de estabelecimento", "condition", "TP_UNID",
          data.frame(
            id = c("Atenção primária", "Hospitais e unidades mistas", "Urgência e pronto atendimento"),
            value = c("primary_care", "hospital", "emergency"),
            stringsAsFactors = FALSE
          ),
          matcher = "cnes_group"
        ),
        municipio_estabelecimento = lookup_filter(
          "Município do estabelecimento", "territory", "CODUFMUN", "municipality"
        )
      ),
      max_periods_uf = 12L,
      max_periods_national = 1L,
      uf_files = TRUE
    ),
    "cnes/leitos_internacao" = list(
      start_year = 2008L,
      geo_column = "CODUFMUN",
      geography_semantics = "Município do estabelecimento",
      period_kind = "yyyymm",
      period_column = "COMPETEN",
      measures = microdata_measure_table(
        c("Leitos existentes", "Leitos SUS", "Leitos não SUS", "Estabelecimentos com leitos"),
        c("leitos_existentes", "leitos_sus", "leitos_nao_sus", "estabelecimentos_com_leitos"),
        c("QT_EXIST", "QT_SUS", "QT_NSUS", "CNES"),
        c("sum", "sum", "sum", "distinct"),
        "stock", c("leitos", "leitos", "leitos", "estabelecimentos"),
        100000, FALSE, FALSE
      ),
      ranking = list(column = "CODLEITO", label = "Tipo de leito", lookup = "raw"),
      filters = list(
        municipio_estabelecimento = lookup_filter(
          "Município do estabelecimento", "territory", "CODUFMUN", "municipality"
        )
      ),
      max_periods_uf = 12L,
      max_periods_national = 1L,
      uf_files = TRUE
    ),
    "sinan/dengue" = list(),
    "sinan/chikungunya" = list(),
    "sinan/zika" = list(),
    "sinan/malaria" = list(),
    "sinan/leptospirose" = list(),
    "sinan/chagas" = list(),
    "sinan/leishmaniose_tegumentar" = list(),
    "sinan/leishmaniose_visceral" = list(),
    "sinasc/nascidos_vivos" = list(
      start_year = 1994L,
      geo_column = "CODMUNRES",
      geography_semantics = "Município de residência da mãe",
      period_kind = "year",
      period_column = "DTNASC",
      measures = microdata_measure_table(
        c(
          "Nascidos vivos", "Baixo peso ao nascer", "Prematuridade",
          "Partos cesáreos", "Anomalia congênita registrada"
        ),
        c("nascidos_vivos", "baixo_peso", "prematuridade", "cesarea", "anomalia"),
        c(NA, "PESO", "SEMAGESTAC", "PARTO", "IDANOMAL"),
        c("rows", "share_lt", "share_lt", "share_equals", "share_equals"),
        c("count", rep("proportion", 4)),
        c("nascidos vivos", rep("%", 4)),
        c(1000, rep(1, 4)),
        c(TRUE, rep(FALSE, 4)),
        FALSE,
        c(NA, 2500, 37, 2, 1)
      ),
      ranking = list(column = "PARTO", label = "Tipo de parto", lookup = "birth_delivery"),
      filters = list(
        municipio_residencia = lookup_filter(
          "Município de residência da mãe", "territory", "CODMUNRES", "municipality"
        ),
        idade_materna = range_filter(
          "Faixa etária materna", "IDADEMAE",
          data.frame(
            id = c("10 a 19 anos", "20 a 34 anos", "35 anos ou mais"),
            value = c("10-19", "20-34", "35+")
          )
        ),
        semanas_gestacao = range_filter(
          "Semanas de gestação", "SEMAGESTAC",
          data.frame(
            id = c("Menos de 22", "22 a 36", "37 a 41", "42 ou mais"),
            value = c("0-21", "22-36", "37-41", "42+")
          )
        ),
        sexo = fixed_filter(
          "Sexo do recém-nascido", "condition", "SEXO",
          data.frame(id = c("Masculino", "Feminino"), value = c("1", "2"))
        ),
        raca_cor = fixed_filter(
          "Raça/cor do recém-nascido", "condition", "RACACOR",
          data.frame(
            id = c("Branca", "Preta", "Amarela", "Parda", "Indígena"),
            value = as.character(1:5)
          )
        ),
        tipo_parto = fixed_filter(
          "Tipo de parto", "condition", "PARTO",
          data.frame(id = c("Vaginal", "Cesáreo"), value = c("1", "2"))
        )
      ),
      max_periods_uf = 10L,
      max_periods_national = 3L,
      uf_files = TRUE
    ),
    "sinasc/nascidos_vivos_ocorrencia" = {
      base <- unclass(microdata_source_spec("sinasc", "nascidos_vivos"))
      base$geo_column <- "CODMUNNASC"
      base$geography_semantics <- "Município de ocorrência do nascimento"
      base$filters$municipio_residencia <- NULL
      base$filters$municipio_ocorrencia <- lookup_filter(
        "Município de ocorrência", "territory", "CODMUNNASC", "municipality"
      )
      base
    },
    NULL
  )

  if (is.null(spec)) {
    stop("Adaptador de microdados não configurado para ", key, ".", call. = FALSE)
  }
  if (domain == "sinan") {
    spec <- list(
      start_year = 2007L,
      geo_column = "ID_MN_RESI",
      geography_semantics = "Município de residência",
      period_kind = "year_column",
      period_column = "NU_ANO",
      age_column = "NU_IDADE_N",
      age_coding = "datasus_encoded",
      measures = microdata_measure_table(
        "Notificações", "notificacoes", reducer = "rows", unit = "notificações",
        standardizable = TRUE
      ),
      ranking = list(column = "CLASSI_FIN", label = "Classificação final", lookup = "fixed"),
      filters = list(
        classificacao_final = fixed_filter(
          "Classificação final", "condition", "CLASSI_FIN", notification_choices
        ),
        criterio_confirmacao = fixed_filter(
          "Critério de confirmação", "condition", "CRITERIO", criterion_choices
        ),
        evolucao = fixed_filter("Evolução", "condition", "EVOLUCAO", evolution_choices),
        sexo = fixed_filter(
          "Sexo", "condition", "CS_SEXO",
          data.frame(id = c("Masculino", "Feminino"), value = c("M", "F"))
        ),
        raca_cor = fixed_filter(
          "Raça/cor", "condition", "CS_RACA", race_color_choices(include_ignored = TRUE)
        ),
        ocupacao = lookup_filter("Ocupação", "condition", "ID_OCUPA_N", "occupation"),
        municipio_residencia = lookup_filter(
          "Município de residência", "territory", "ID_MN_RESI", "municipality"
        )
      ),
      max_periods_uf = 3L,
      max_periods_national = 3L,
      uf_files = FALSE
    )
    if (dataset == "malaria") {
      spec$filters$criterio_confirmacao <- NULL
      spec$filters$evolucao <- NULL
    }
  }

  spec <- utils::modifyList(spec, source_config)
  spec$frequency <- get_domain_config(domain)$frequency
  class(spec) <- c("microdata_source_spec", "list")
  spec
}

load_microdatasus_dataset <- function(name) {
  if (exists(name, envir = MICRODATA_LOOKUPS, inherits = FALSE)) {
    return(get(name, envir = MICRODATA_LOOKUPS, inherits = FALSE))
  }
  environment <- new.env(parent = emptyenv())
  utils::data(list = name, package = "microdatasus", envir = environment)
  if (!exists(name, envir = environment, inherits = FALSE)) {
    stop("O pacote microdatasus não contém a tabela ", name, ".", call. = FALSE)
  }
  value <- get(name, envir = environment, inherits = FALSE)
  assign(name, value, envir = MICRODATA_LOOKUPS)
  value
}

microdata_procedure_lookup <- function() {
  name <- "procedure_lookup"
  if (exists(name, envir = MICRODATA_LOOKUPS, inherits = FALSE)) {
    return(get(name, envir = MICRODATA_LOOKUPS, inherits = FALSE))
  }
  data <- load_microdatasus_dataset("sigtab")
  result <- data.frame(
    code = as.character(data$COD),
    label = stringi::stri_unescape_unicode(as.character(data$nome_proced)),
    stringsAsFactors = FALSE
  )
  assign(name, result, envir = MICRODATA_LOOKUPS)
  result
}

microdata_occupation_lookup <- function() {
  name <- "occupation_lookup"
  if (exists(name, envir = MICRODATA_LOOKUPS, inherits = FALSE)) {
    return(get(name, envir = MICRODATA_LOOKUPS, inherits = FALSE))
  }
  cbo <- load_microdatasus_dataset("tabCBO")
  legacy <- load_microdatasus_dataset("tabOcupacao")
  result <- rbind(
    data.frame(code = as.character(cbo$cod), label = as.character(cbo$nome)),
    data.frame(code = as.character(legacy$cod), label = as.character(legacy$nome))
  )
  result$label <- stringi::stri_unescape_unicode(result$label)
  result <- result[!duplicated(result$code), , drop = FALSE]
  assign(name, result, envir = MICRODATA_LOOKUPS)
  result
}

microdata_municipality_lookup <- function(uf = NULL) {
  name <- "municipality_lookup"
  if (exists(name, envir = MICRODATA_LOOKUPS, inherits = FALSE)) {
    result <- get(name, envir = MICRODATA_LOOKUPS, inherits = FALSE)
  } else {
    data <- load_microdatasus_dataset("tabMun")
    code6 <- format_integer_code(data$munResCod, 6L)
    keep <- !is.na(code6) & data$munResTipo == "MUNIC" & data$munResStatus == "ATIVO"
    result <- data.frame(
      code6 = code6[keep],
      code7 = normalize_municipality_code(code6[keep]),
      label = stringi::stri_unescape_unicode(as.character(data$munResNome[keep])),
      stringsAsFactors = FALSE
    )
    assign(name, result, envir = MICRODATA_LOOKUPS)
  }
  if (!is.null(uf) && nzchar(uf)) {
    result <- result[substr(result$code6, 1L, 2L) == uf_code(uf), , drop = FALSE]
  }
  result
}

microdata_cnes_unit_lookup <- function() {
  name <- "cnes_unit_type"
  if (exists(name, envir = MICRODATA_LOOKUPS, inherits = FALSE)) {
    return(get(name, envir = MICRODATA_LOOKUPS, inherits = FALSE))
  }
  raw <- data.frame(TP_UNID = sprintf("%02d", 1:99), stringsAsFactors = FALSE)
  processed <- microdatasus::process_cnes(
    raw,
    information_system = "CNES-ST",
    municipality_data = FALSE
  )
  result <- data.frame(
    code = raw$TP_UNID,
    label = as.character(processed$TP_UNID),
    stringsAsFactors = FALSE
  )
  unchanged <- result$code == result$label
  result$label[unchanged] <- paste("Tipo de unidade", result$code[unchanged])
  assign(name, result, envir = MICRODATA_LOOKUPS)
  result
}

lookup_to_options <- function(data) {
  data.frame(
    id = paste(data$code, data$label, sep = " - "),
    value = data$code,
    stringsAsFactors = FALSE
  )
}

microdata_filter_choices <- function(definition, uf = NULL) {
  switch(
    definition$choice_type,
    fixed = definition$choices,
    icd = ICD10_CHAPTERS,
    procedure = lookup_to_options(microdata_procedure_lookup()),
    occupation = lookup_to_options(microdata_occupation_lookup()),
    municipality = {
      municipalities <- microdata_municipality_lookup(uf)
      data.frame(
        id = paste(municipalities$code6, municipalities$label),
        value = municipalities$code6,
        stringsAsFactors = FALSE
      )
    },
    stop("Tipo de filtro de microdados desconhecido.", call. = FALSE)
  )
}

empty_filter_choices <- function() {
  data.frame(id = character(), value = character(), stringsAsFactors = FALSE)
}

microdata_period_options <- function(spec, today = Sys.Date()) {
  current_year <- as.integer(format(today, "%Y"))
  if (spec$frequency %in% c("monthly", "snapshot")) {
    first <- as.Date(sprintf("%d-01-01", spec$start_year))
    current_month <- as.Date(format(today, "%Y-%m-01"))
    latest <- seq.Date(current_month, by = "-1 month", length.out = 3L)[[3L]]
    dates <- seq.Date(first, latest, by = "month")
    month_labels <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    return(data.frame(
      id = paste0(month_labels[as.integer(format(dates, "%m"))], "/", format(dates, "%Y")),
      value = format(dates, "%Y%m"),
      stringsAsFactors = FALSE
    ))
  }
  years <- seq.int(spec$start_year, current_year - 2L)
  data.frame(id = as.character(years), value = as.character(years), stringsAsFactors = FALSE)
}

fetch_microdata_options <- function(domain, dataset, uf = NULL, geo_level = "state", refresh = FALSE) {
  if (identical(uf, "all") || identical(uf, "")) uf <- NULL
  spec <- microdata_source_spec(domain, dataset)
  filters <- lapply(spec$filters, function(definition) {
    if (definition$choice_type %in% c("fixed", "icd")) {
      microdata_filter_choices(definition, uf)
    } else {
      empty_filter_choices()
    }
  })
  max_periods <- if (is.null(uf)) spec$max_periods_national else spec$max_periods_uf
  options <- list(
    linha = data.frame(
      id = c("Unidade da Federação", "Município", "Região de Saúde", spec$ranking$label, "Período"),
      value = c("state", "municipality", "health_region", "ranking", "period"),
      stringsAsFactors = FALSE
    ),
    coluna = data.frame(id = "Não ativa", value = "-", stringsAsFactors = FALSE),
    conteudo = spec$measures[c("id", "value")],
    periodo = microdata_period_options(spec),
    filtros = filters,
    filter_labels = vapply(spec$filters, `[[`, character(1), "label"),
    filter_roles = vapply(spec$filters, `[[`, character(1), "role"),
    filter_choice_types = vapply(spec$filters, `[[`, character(1), "choice_type"),
    max_periods = max_periods,
    provider_label = "Microdados DBC (datasusr com contingência microdatasus)",
    source = list(
      source = spec$source,
      file_type = spec$file_type,
      information_system = spec$information_system
    )
  )
  attr(options, "provider") <- "microdata"
  attr(options, "geo_level") <- geo_level
  attr(options, "refresh") <- isTRUE(refresh)
  class(options) <- c("microdata_options", "datasus_options", "list")
  options
}

microdata_period_parts <- function(query) {
  text <- paste(query$periods$id, query$periods$value)
  if (query$frequency %in% c("monthly", "snapshot")) {
    key <- explicit_period_month(text)
    if (any(is.na(key))) stop("Há competências mensais inválidas na consulta.", call. = FALSE)
    return(data.frame(
      label = query$periods$id,
      key = sprintf("%06d", key),
      year = key %/% 100L,
      month = key %% 100L,
      stringsAsFactors = FALSE
    ))
  }
  year <- extract_year(text)
  if (any(is.na(year))) stop("Há anos inválidos na consulta.", call. = FALSE)
  data.frame(
    label = query$periods$id,
    key = as.character(year),
    year = year,
    month = NA_integer_,
    stringsAsFactors = FALSE
  )
}

microdata_required_columns <- function(spec) {
  measure_columns <- stats::na.omit(spec$measures$column)
  filter_columns <- vapply(spec$filters, `[[`, character(1), "column")
  period_columns <- spec$period_columns %||% spec$period_column
  unique(c(
    spec$geo_column,
    spec$ranking$column,
    period_columns,
    measure_columns,
    filter_columns,
    spec$age_column %||% character(),
    spec$age_unit_column %||% character()
  ))
}

microdata_query_columns <- function(spec, query = NULL) {
  if (is.null(query)) return(microdata_required_columns(spec))

  measure <- spec$measures[spec$measures$value == query$measure_value, , drop = FALSE]
  if (nrow(measure) != 1L) {
    stop("Medida de microdados desconhecida: ", query$measure_value, ".", call. = FALSE)
  }
  active_filters <- names(query$filters)[vapply(query$filters, function(values) {
    length(setdiff(as.character(values), c("", "all"))) > 0L
  }, logical(1))]
  if (isTRUE(query$urgent_only)) {
    urgency <- names(spec$filters)[vapply(
      spec$filters,
      function(definition) identical(definition$role, "urgency"),
      logical(1)
    )]
    active_filters <- unique(c(active_filters, urgency))
  }
  age_column <- if (identical(query$scale, "age_standardized_rate")) {
    c(
      spec$age_column %||% character(),
      spec$age_unit_column %||% character()
    )
  } else {
    character()
  }
  unknown_filters <- setdiff(active_filters, names(spec$filters))
  if (length(unknown_filters) > 0L) {
    stop("Filtro sem adaptador de microdados: ", unknown_filters[[1L]], ".", call. = FALSE)
  }
  filter_columns <- if (length(active_filters) > 0L) {
    vapply(spec$filters[active_filters], `[[`, character(1), "column")
  } else {
    character()
  }
  unique(c(
    spec$geo_column,
    spec$ranking$column,
    spec$period_columns %||% spec$period_column,
    stats::na.omit(measure$column),
    filter_columns,
    age_column
  ))
}

microdata_query_ufs <- function(query, spec) {
  if (!isTRUE(spec$uf_files)) return(NA_character_)
  if (!is.null(query$uf)) return(query$uf)

  territory_codes <- query_territory_codes(query)
  if (length(territory_codes) == 0L) return(datasusr::datasus_ufs())
  state_codes <- unique(substr(territory_codes, 1L, 2L))
  ufs <- datasusr::datasus_ufs()
  matched <- match(state_codes, vapply(ufs, uf_code, character(1)))
  if (any(is.na(matched))) {
    stop("O filtro territorial contém uma UF sem adaptador de microdados.", call. = FALSE)
  }
  unname(ufs[matched])
}

microdata_dbc_cache_directory <- function() {
  path <- file.path(cache_directory(), "dbc")
  ensure_directory(path)
  path
}

microdata_expected_file_names <- function(spec, parts, uf = NULL) {
  arguments <- list(
    source = spec$source,
    file_type = spec$file_type,
    year = unique(parts$year),
    include_prelim = TRUE,
    check_exists = FALSE,
    verbose = FALSE
  )
  if (spec$frequency %in% c("monthly", "snapshot")) {
    arguments$month <- unique(parts$month)
  }
  if (isTRUE(spec$uf_files)) arguments$uf <- uf
  files <- do.call(datasusr::datasus_list_files, arguments)
  unique(toupper(as.character(files$file_name)))
}

microdata_dbc_cache_path <- function(file) {
  url_hash <- digest::digest(
    as.character(file$url[[1L]]),
    algo = "sha256",
    serialize = FALSE
  )
  directory <- file.path(
    microdata_dbc_cache_directory(),
    url_hash,
    as.character(file$source[[1L]]),
    as.character(file$file_type[[1L]])
  )
  ensure_directory(directory)
  file.path(directory, as.character(file$file_name[[1L]]))
}

microdata_dbc_metadata_path <- function(path) {
  paste0(path, ".metadata.rds")
}

read_microdata_dbc_metadata <- function(path) {
  metadata_path <- microdata_dbc_metadata_path(path)
  if (!file.exists(metadata_path)) return(NULL)
  tryCatch(readRDS(metadata_path), error = function(error) NULL)
}

microdata_cached_dbc_is_fresh <- function(path, url) {
  metadata <- read_microdata_dbc_metadata(path)
  file_valid <- file.exists(path) && is.finite(file.info(path)$size) && file.info(path)$size > 0L
  metadata_valid <- !is.null(metadata) && identical(metadata$url, as.character(url)) &&
    is.character(metadata$sha256) && length(metadata$sha256) == 1L &&
    isTRUE(all.equal(unname(file.info(path)$size), metadata$size))
  file_valid && metadata_valid && cache_is_fresh(path, MICRODATA_DBC_CACHE_MAX_AGE) &&
    identical(
      digest::digest(file = path, algo = "sha256", serialize = FALSE),
      metadata$sha256
    )
}

download_microdata_dbc <- function(
  file,
  refresh = FALSE,
  download_function = datasusr::datasus_download
) {
  target <- microdata_dbc_cache_path(file)
  metadata <- read_microdata_dbc_metadata(target)
  fresh <- !isTRUE(refresh) && microdata_cached_dbc_is_fresh(target, file$url[[1L]])

  if (!fresh) {
    staging <- tempfile(pattern = ".dbc-download-", tmpdir = microdata_dbc_cache_directory())
    ensure_directory(staging)
    on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
    downloaded <- download_function(
      files = file,
      use_cache = FALSE,
      dest_dir = staging,
      overwrite = TRUE,
      timeout = 240,
      verbose = FALSE
    )
    candidate <- downloaded$local_file[[1L]]
    candidate_valid <- file.exists(candidate) && is.finite(file.info(candidate)$size) &&
      file.info(candidate)$size > 0L
    if (!candidate_valid) {
      stop("O download DBC não produziu um arquivo válido.", call. = FALSE)
    }

    temporary <- tempfile(pattern = ".dbc-cache-", tmpdir = dirname(target))
    on.exit(unlink(temporary), add = TRUE)
    if (!file.copy(candidate, temporary, overwrite = TRUE)) {
      stop("Não foi possível preparar o arquivo DBC para o cache.", call. = FALSE)
    }
    if (!file.rename(temporary, target) && !file.copy(temporary, target, overwrite = TRUE)) {
      stop("Não foi possível atualizar o cache DBC.", call. = FALSE)
    }
    metadata <- list(
      url = as.character(file$url[[1L]]),
      release = as.character(file$period[[1L]]),
      retrieved_at = Sys.time(),
      size = unname(file.info(target)$size),
      sha256 = digest::digest(file = target, algo = "sha256", serialize = FALSE)
    )
    saveRDS(metadata, microdata_dbc_metadata_path(target), version = 3)
  }

  dplyr::mutate(
    file,
    local_file = target,
    downloaded = !fresh,
    checksum_sha256 = metadata$sha256,
    retrieved_at = metadata$retrieved_at
  )
}

download_microdata_dbcs <- function(files, refresh = FALSE) {
  dplyr::bind_rows(lapply(seq_len(nrow(files)), function(index) {
    download_microdata_dbc(files[index, , drop = FALSE], refresh)
  }))
}

standardize_microdata_columns <- function(data) {
  data <- tibble::as_tibble(data)
  names(data) <- toupper(names(data))
  if ("SOURCE" %in% names(data) && !".SOURCE_FILE" %in% names(data)) {
    data$.SOURCE_FILE <- as.character(data$SOURCE)
    data$SOURCE <- NULL
  }
  data
}

fetch_with_datasusr <- function(
  spec,
  parts,
  uf = NULL,
  refresh = FALSE,
  columns = microdata_required_columns(spec)
) {
  columns <- tolower(columns)
  rows <- lapply(split(parts, parts$year), function(periods) {
    list_arguments <- list(
      source = spec$source,
      file_type = spec$file_type,
      year = unique(periods$year),
      include_prelim = TRUE,
      timeout = 240,
      verbose = FALSE
    )
    if (spec$frequency %in% c("monthly", "snapshot")) {
      list_arguments$month <- unique(periods$month)
    }
    if (isTRUE(spec$uf_files)) list_arguments$uf <- uf
    files <- do.call(datasusr::datasus_list_files, list_arguments)
    if (nrow(files) == 0L) {
      stop("Nenhum arquivo DBC foi publicado para o período solicitado.", call. = FALSE)
    }
    publication_priority <- match(files$period, c("current", "historical", "prelim"), nomatch = 99L)
    files <- files[order(publication_priority), , drop = FALSE]
    files <- files[!duplicated(files$file_name), , drop = FALSE]
    expected_files <- microdata_expected_file_names(spec, periods, uf)
    listed_files <- toupper(as.character(files$file_name))
    if (!setequal(listed_files, expected_files)) {
      stop(
        "A listagem DBC não corresponde aos arquivos esperados: ",
        paste(expected_files, collapse = ", "), ".",
        call. = FALSE
      )
    }
    downloads <- download_microdata_dbcs(files, refresh)
    available <- downloads$local_file[file.exists(downloads$local_file)]
    if (length(available) != nrow(downloads)) {
      stop("O download DBC terminou sem todos os arquivos solicitados.", call. = FALSE)
    }
    data <- dplyr::bind_rows(lapply(available, function(file) {
      records <- datasusr::read_datasus_dbc(file, select = columns, verbose = FALSE)
      records$.source_file <- basename(file)
      records
    }))
    if (nrow(data) > 0L) data$.source_year <- unique(periods$year)[[1L]]
    manifest <- dplyr::transmute(
      downloads,
      file = .data$file_name,
      url = .data$url,
      release = .data$period,
      sha256 = .data$checksum_sha256,
      retrieved_at = .data$retrieved_at
    )
    list(data = data, manifest = manifest)
  })
  result <- standardize_microdata_columns(dplyr::bind_rows(lapply(rows, `[[`, "data")))
  attr(result, "source_manifest") <- dplyr::bind_rows(lapply(rows, `[[`, "manifest"))
  result
}

fetch_with_microdatasus <- function(
  spec,
  parts,
  uf = NULL,
  columns = microdata_required_columns(spec),
  fetch_function = microdatasus::fetch_datasus
) {
  period_rows <- if (spec$frequency %in% c("monthly", "snapshot")) {
    split(parts, seq_len(nrow(parts)))
  } else {
    split(parts, parts$year)
  }
  rows <- lapply(period_rows, function(periods) {
    arguments <- list(
      year_start = min(periods$year),
      year_end = max(periods$year),
      uf = if (isTRUE(spec$uf_files)) uf else "all",
      information_system = spec$information_system,
      vars = columns,
      stop_on_error = TRUE,
      timeout = 240,
      track_source = TRUE,
      quiet = TRUE
    )
    if (spec$frequency %in% c("monthly", "snapshot")) {
      arguments$month_start <- periods$month[[1L]]
      arguments$month_end <- periods$month[[1L]]
    }
    result <- do.call(fetch_function, arguments)
    if (is.null(result) || nrow(result) == 0L) {
      stop(
        "A contingência não retornou o arquivo esperado para ", periods$label[[1L]], ".",
        call. = FALSE
      )
    }
    result <- standardize_microdata_columns(result)
    result$.SOURCE_YEAR <- periods$year[[1L]]
    if (!".SOURCE_FILE" %in% names(result)) {
      stop("A contingência não informou a identidade do arquivo DBC.", call. = FALSE)
    }
    expected_files <- microdata_expected_file_names(spec, periods, uf)
    source_files <- unique(toupper(basename(as.character(result$.SOURCE_FILE))))
    if (!setequal(source_files, expected_files)) {
      stop(
        "A contingência retornou arquivo diferente do esperado para ",
        periods$label[[1L]], ".",
        call. = FALSE
      )
    }
    manifest <- tibble::tibble(
      file = unique(as.character(result$.SOURCE_FILE)),
      url = NA_character_,
      release = "microdatasus",
      sha256 = NA_character_,
      retrieved_at = Sys.time()
    )
    list(data = result, manifest = manifest)
  })
  result <- standardize_microdata_columns(dplyr::bind_rows(lapply(rows, `[[`, "data")))
  attr(result, "source_manifest") <- dplyr::bind_rows(lapply(rows, `[[`, "manifest"))
  result
}

fetch_microdata_slice <- function(spec, parts, uf = NULL, refresh = FALSE, query = NULL) {
  columns <- microdata_query_columns(spec, query)
  primary <- tryCatch(
    fetch_with_datasusr(spec, parts, uf, refresh, columns),
    error = function(error) error
  )
  if (!inherits(primary, "error") && nrow(primary) > 0L) {
    source_files <- if (".SOURCE_FILE" %in% names(primary)) unique(primary$.SOURCE_FILE) else character()
    return(list(
      data = primary,
      provider = "datasusr",
      source_files = source_files,
      source_manifest = attr(primary, "source_manifest") %||% tibble::tibble(),
      warnings = character()
    ))
  }

  fallback <- tryCatch(
    fetch_with_microdatasus(spec, parts, uf, columns),
    error = function(error) error
  )
  if (!inherits(fallback, "error") && nrow(fallback) > 0L) {
    reason <- if (inherits(primary, "error")) conditionMessage(primary) else "nenhum arquivo foi retornado"
    source_files <- if (".SOURCE_FILE" %in% names(fallback)) unique(fallback$.SOURCE_FILE) else character()
    return(list(
      data = fallback,
      provider = "microdatasus",
      source_files = source_files,
      source_manifest = attr(fallback, "source_manifest") %||% tibble::tibble(),
      warnings = paste("datasusr indisponível; microdatasus usado como contingência:", reason)
    ))
  }

  primary_message <- if (inherits(primary, "error")) conditionMessage(primary) else "nenhum registro retornado"
  fallback_message <- if (inherits(fallback, "error")) conditionMessage(fallback) else "nenhum registro retornado"
  stop(
    "Nenhum provedor conseguiu ler os arquivos DBC. datasusr: ", primary_message,
    "; microdatasus: ", fallback_message,
    call. = FALSE
  )
}

normalize_microdata_code <- function(x) {
  toupper(gsub("[^A-Z0-9]", "", trimws(as.character(x))))
}

match_icd_ranges <- function(codes, ranges) {
  full_codes <- normalize_microdata_code(codes)
  category_codes <- substr(full_codes, 1L, 3L)
  matches <- rep(FALSE, length(full_codes))
  for (range in ranges) {
    limits <- strsplit(range, "-", fixed = TRUE)[[1L]]
    if (length(limits) == 2L) {
      matches <- matches | (
        !is.na(category_codes) & category_codes >= limits[[1L]] & category_codes <= limits[[2L]]
      )
    } else {
      prefix <- normalize_microdata_code(limits[[1L]])
      matches <- matches | (!is.na(full_codes) & startsWith(full_codes, prefix))
    }
  }
  matches
}

match_cnes_groups <- function(codes, groups) {
  code_groups <- list(
    primary_care = FACILITY_TYPE_CODES$primary_care,
    hospital = FACILITY_TYPE_CODES$hospital,
    emergency = FACILITY_TYPE_CODES$emergency
  )
  accepted <- unique(unlist(code_groups[groups], use.names = FALSE))
  normalize_microdata_code(codes) %in% accepted
}

match_numeric_ranges <- function(values, ranges) {
  values <- suppressWarnings(as.numeric(as.character(values)))
  matched <- rep(FALSE, length(values))
  for (range in ranges) {
    if (endsWith(range, "+")) {
      lower <- suppressWarnings(as.numeric(sub("[+]$", "", range)))
      matched <- matched | (!is.na(values) & values >= lower)
    } else {
      limits <- suppressWarnings(as.numeric(strsplit(range, "-", fixed = TRUE)[[1L]]))
      if (length(limits) == 2L && all(is.finite(limits))) {
        matched <- matched | (!is.na(values) & values >= limits[[1L]] & values <= limits[[2L]])
      }
    }
  }
  matched
}

apply_microdata_filters <- function(data, query, spec) {
  filters <- query$filters
  if (isTRUE(query$urgent_only)) {
    urgency <- Filter(function(definition) definition$role == "urgency", spec$filters)
    if (length(urgency) == 0L) {
      stop("A fonte selecionada não possui filtro de urgência.", call. = FALSE)
    }
    field <- names(urgency)[[1L]]
    filters[[field]] <- unique(c(filters[[field]] %||% character(), urgency[[1L]]$urgent_values))
  }

  for (field in names(filters)) {
    values <- setdiff(as.character(filters[[field]]), c("", "all"))
    if (length(values) == 0L) next
    definition <- spec$filters[[field]]
    if (is.null(definition)) stop("Filtro sem adaptador de microdados: ", field, ".", call. = FALSE)
    if (!definition$column %in% names(data)) {
      stop("A coluna necessária ao filtro não existe no arquivo: ", definition$column, ".", call. = FALSE)
    }
    keep <- switch(
      definition$matcher,
      exact = normalize_microdata_code(data[[definition$column]]) %in% normalize_microdata_code(values),
      icd = match_icd_ranges(data[[definition$column]], values),
      cnes_group = match_cnes_groups(data[[definition$column]], values),
      numeric_range = match_numeric_ranges(data[[definition$column]], values),
      stop("Método de filtro de microdados desconhecido.", call. = FALSE)
    )
    data <- data[!is.na(keep) & keep, , drop = FALSE]
  }

  if (!is.null(query$uf) && spec$geo_column %in% names(data)) {
    municipality <- normalize_microdata_code(data[[spec$geo_column]])
    data <- data[substr(municipality, 1L, 2L) == uf_code(query$uf), , drop = FALSE]
  }
  data
}

microdata_record_period_key <- function(data, spec) {
  switch(
    spec$period_kind,
    year_month = sprintf(
      "%04d%02d",
      suppressWarnings(as.integer(data[[spec$period_columns[[1L]]]])),
      suppressWarnings(as.integer(data[[spec$period_columns[[2L]]]]))
    ),
    yyyymm = sprintf("%06d", suppressWarnings(as.integer(data[[spec$period_column]]))),
    year = {
      value <- gsub("[^0-9]", "", as.character(data[[spec$period_column]]))
      result <- substr(value, pmax(1L, nchar(value) - 3L), nchar(value))
      if (".SOURCE_YEAR" %in% names(data)) {
        fallback <- !grepl("^(19|20)[0-9]{2}$", result)
        result[fallback] <- as.character(data$.SOURCE_YEAR[fallback])
      }
      result
    },
    year_column = sprintf("%04d", suppressWarnings(as.integer(data[[spec$period_column]]))),
    stop("Regra temporal de microdados desconhecida.", call. = FALSE)
  )
}

state_name_from_code <- function(code) {
  choices <- uf_choices()
  keep <- unname(choices) != "all"
  state_codes <- vapply(unname(choices[keep]), uf_code, character(1))
  unname(names(choices)[keep][match(code, state_codes)])
}

microdata_geography_labels <- function(data, query, spec, refresh = FALSE) {
  raw_code <- normalize_microdata_code(data[[spec$geo_column]])
  code6 <- ifelse(nchar(raw_code) >= 6L, substr(raw_code, 1L, 6L), NA_character_)
  code7 <- normalize_municipality_code(code6)
  municipality <- microdata_municipality_lookup(query$uf)
  municipality_name <- municipality$label[match(code7, municipality$code7)]

  if (query$geo_level == "state") {
    state_code <- substr(code6, 1L, 2L)
    state_name <- state_name_from_code(state_code)
    result <- paste(state_code, state_name)
    result[is.na(state_code) | is.na(state_name)] <- NA_character_
    return(result)
  }
  if (query$geo_level == "municipality") {
    result <- paste(code7, municipality_name)
    result[is.na(code7) | is.na(municipality_name)] <- NA_character_
    return(result)
  }

  year <- analysis_reference_year(query)
  crosswalk <- load_health_region_crosswalk(year, query$uf, refresh)
  region_code <- crosswalk$geo_code[match(code7, crosswalk$municipality_code)]
  region_name <- if ("geo_name" %in% names(crosswalk)) {
    crosswalk$geo_name[match(code7, crosswalk$municipality_code)]
  } else {
    rep(NA_character_, length(region_code))
  }
  result <- paste(region_code, region_name)
  result[is.na(region_code) | is.na(region_name)] <- NA_character_
  result
}

microdata_geography_universe <- function(query, refresh = FALSE) {
  selected_codes <- query_territory_codes(query)
  if (!is.null(query$uf) && length(selected_codes) > 0L) {
    selected_codes <- selected_codes[substr(selected_codes, 1L, 2L) == uf_code(query$uf)]
  }

  if (query$geo_level == "state") {
    state_codes <- if (length(selected_codes) > 0L) {
      unique(substr(selected_codes, 1L, 2L))
    } else if (!is.null(query$uf)) {
      uf_code(query$uf)
    } else {
      vapply(datasusr::datasus_ufs(), uf_code, character(1))
    }
    state_names <- state_name_from_code(state_codes)
    return(tibble::tibble(
      geo_code = state_codes,
      geo_name = state_names,
      label = paste(state_codes, state_names)
    ))
  }

  if (query$geo_level == "municipality") {
    municipalities <- microdata_municipality_lookup(query$uf)
    if (length(selected_codes) > 0L) {
      expected <- tibble::tibble(geo_code = selected_codes)
      municipalities <- dplyr::left_join(
        expected,
        dplyr::transmute(municipalities, geo_code = .data$code7, geo_name = .data$label),
        by = "geo_code"
      )
      municipalities$geo_name[is.na(municipalities$geo_name)] <- paste(
        "Município", municipalities$geo_code[is.na(municipalities$geo_name)]
      )
    } else {
      municipalities <- dplyr::transmute(
        municipalities,
        geo_code = .data$code7,
        geo_name = .data$label
      )
    }
    municipalities$label <- paste(municipalities$geo_code, municipalities$geo_name)
    return(dplyr::distinct(municipalities, .data$geo_code, .keep_all = TRUE))
  }

  crosswalk <- load_health_region_crosswalk(
    analysis_reference_year(query), query$uf, refresh
  )
  if (length(selected_codes) > 0L) {
    crosswalk <- crosswalk[crosswalk$municipality_code %in% selected_codes, , drop = FALSE]
  }
  regions <- dplyr::distinct(crosswalk, .data$geo_code, .data$geo_name)
  regions <- regions[!is.na(regions$geo_code) & !is.na(regions$geo_name), , drop = FALSE]
  regions$label <- paste(regions$geo_code, regions$geo_name)
  regions
}

complete_microdata_dimensions <- function(bundle, query, refresh = FALSE) {
  universe <- microdata_geography_universe(query, refresh)
  missing_value <- if (identical(bundle$measure$measure_type[[1L]], "proportion")) NA_real_ else 0
  missing_labels <- setdiff(universe$label, bundle$map$label)
  if (length(missing_labels) > 0L) {
    source_line <- if (nrow(bundle$map) > 0L) bundle$map$source_line[[1L]] else "Território"
    bundle$map <- dplyr::bind_rows(
      bundle$map,
      tibble::tibble(
        dimension = "geography",
        label = missing_labels,
        value = missing_value,
        is_total = FALSE,
        source_line = source_line
      )
    )
  }

  expected_labels <- unique(as.character(query$periods$id))
  observed <- match(expected_labels, bundle$series$label)
  period_values <- rep(missing_value, length(expected_labels))
  period_values[!is.na(observed)] <- bundle$series$value[observed[!is.na(observed)]]
  completed_series <- tibble::tibble(
    dimension = "period",
    label = expected_labels,
    value = period_values,
    is_total = FALSE,
    source_line = "Período"
  )
  extra <- bundle$series[!(bundle$series$label %in% expected_labels), , drop = FALSE]
  bundle$series <- dplyr::bind_rows(completed_series, extra)
  bundle
}

microdata_lookup_labels <- function(codes, lookup, definition = NULL) {
  codes <- normalize_microdata_code(codes)
  unique_codes <- unique(codes)
  table <- switch(
    lookup,
    procedure = microdata_procedure_lookup(),
    occupation = microdata_occupation_lookup(),
    cnes_unit = microdata_cnes_unit_lookup(),
    birth_delivery = data.frame(
      code = c("1", "2", "9"),
      label = c("Vaginal", "Cesáreo", "Ignorado"),
      stringsAsFactors = FALSE
    ),
    NULL
  )
  if (!is.null(table)) {
    labels <- table$label[match(unique_codes, normalize_microdata_code(table$code))]
  } else if (lookup == "icd") {
    chapter <- vapply(unique_codes, function(code) {
      matched <- which(vapply(
        ICD10_CHAPTERS$value,
        function(range) match_icd_ranges(code, range),
        logical(1)
      ))
      if (length(matched) == 0L) return(NA_character_)
      ICD10_CHAPTERS$id[[matched[[1L]]]]
    }, character(1))
    labels <- chapter
  } else if (lookup == "fixed" && !is.null(definition)) {
    labels <- definition$id[match(unique_codes, normalize_microdata_code(definition$value))]
  } else {
    labels <- rep(NA_character_, length(unique_codes))
  }
  labels[is.na(labels) | !nzchar(labels)] <- "Sem descrição"
  unique_results <- paste(unique_codes, labels, sep = " - ")
  unique_results[is.na(unique_codes) | !nzchar(unique_codes)] <- "Sem informação"
  unique_results[match(codes, unique_codes)]
}

microdata_measure_spec <- function(spec, value) {
  measure <- spec$measures[spec$measures$value == value, , drop = FALSE]
  if (nrow(measure) != 1L) stop("Medida de microdados desconhecida: ", value, ".", call. = FALSE)
  measure
}

reduce_measure_details <- function(data, measure) {
  if (measure$reducer == "rows") {
    return(list(
      value = as.numeric(nrow(data)), numerator = NA_real_,
      eligible = as.numeric(nrow(data)), missing = 0
    ))
  }
  column <- measure$column[[1L]]
  if (!column %in% names(data)) stop("A medida exige a coluna ausente ", column, ".", call. = FALSE)
  values <- data[[column]]
  if (measure$reducer == "distinct") {
    values <- normalize_microdata_code(values)
    known <- !is.na(values) & nzchar(values)
    return(list(
      value = as.numeric(length(unique(values[known]))), numerator = NA_real_,
      eligible = sum(known), missing = sum(!known)
    ))
  }
  if (measure$reducer %in% c("share_lt", "share_equals")) {
    numeric_values <- suppressWarnings(as.numeric(as.character(values)))
    known <- is.finite(numeric_values)
    if (measure$reducer == "share_equals") {
      known <- known & numeric_values %in% c(1, 2)
      numerator <- sum(known & numeric_values == measure$parameter[[1L]])
    } else {
      numerator <- sum(known & numeric_values < measure$parameter[[1L]])
    }
    eligible <- sum(known)
    return(list(
      value = if (eligible == 0L) NA_real_ else numerator / eligible * 100,
      numerator = as.numeric(numerator), eligible = as.numeric(eligible),
      missing = as.numeric(length(values) - eligible)
    ))
  }
  numeric_values <- suppressWarnings(as.numeric(as.character(values)))
  if (length(numeric_values) == 0L) {
    return(list(value = 0, numerator = NA_real_, eligible = 0, missing = 0))
  }
  known <- is.finite(numeric_values)
  list(
    value = if (any(known)) sum(numeric_values[known]) else NA_real_,
    numerator = NA_real_, eligible = sum(known), missing = sum(!known)
  )
}

reduce_microdata_measure <- function(data, measure) {
  reduce_measure_details(data, measure)$value
}

aggregate_microdata_dimension <- function(data, labels, measure, dimension, source_line) {
  valid <- !is.na(labels) & nzchar(trimws(labels))
  data <- data[valid, , drop = FALSE]
  labels <- as.character(labels[valid])
  if (nrow(data) == 0L) {
    return(tibble::tibble(
      dimension = character(), label = character(), value = numeric(),
      numerator = numeric(), eligible = numeric(), missing = numeric(),
      measure_type = character(), is_total = logical(), source_line = character()
    ))
  }
  groups <- split(seq_len(nrow(data)), labels)
  result <- dplyr::bind_rows(lapply(names(groups), function(label) {
    details <- reduce_measure_details(data[groups[[label]], , drop = FALSE], measure)
    tibble::tibble(
      label = label,
      value = details$value,
      numerator = details$numerator,
      eligible = details$eligible,
      missing = details$missing
    )
  }))
  result$dimension <- dimension
  result$measure_type <- measure$measure_type[[1L]]
  result$is_total <- FALSE
  result$source_line <- source_line
  dplyr::select(
    result, "dimension", "label", "value", "numerator", "eligible", "missing",
    "measure_type", "is_total", "source_line"
  )
}

decode_datasus_age <- function(x, coding = "years", unit = NULL) {
  values <- suppressWarnings(as.integer(as.character(x)))
  if (coding == "years") return(ifelse(values >= 0L & values <= 130L, values, NA_integer_))
  if (coding == "sih_unit") {
    units <- suppressWarnings(as.integer(as.character(unit)))
    if (length(units) != length(values)) return(rep(NA_integer_, length(values)))
    result <- rep(NA_integer_, length(values))
    result[units %in% c(2L, 3L)] <- 0L
    result[units == 4L] <- values[units == 4L]
    result[units == 5L] <- 100L + values[units == 5L]
    result[result < 0L | result > 130L] <- NA_integer_
    return(result)
  }
  text <- as.character(values)
  unit <- suppressWarnings(as.integer(substr(text, 1L, 1L)))
  amount <- suppressWarnings(as.integer(substr(text, 2L, nchar(text))))
  result <- rep(NA_integer_, length(values))
  result[unit %in% 1:3] <- 0L
  result[unit == 4L] <- amount[unit == 4L]
  result[unit == 5L] <- 100L + amount[unit == 5L]
  result[result < 0L | result > 130L] <- NA_integer_
  result
}

decode_source_age <- function(data, spec) {
  unit <- if (!is.null(spec$age_unit_column) && spec$age_unit_column %in% names(data)) {
    data[[spec$age_unit_column]]
  } else {
    NULL
  }
  decode_datasus_age(
    data[[spec$age_column]],
    spec$age_coding %||% "years",
    unit = unit
  )
}

age_group_quinquennial <- function(age) {
  breaks <- c(seq(0, 80, by = 5), Inf)
  labels <- c(paste(seq(0, 75, by = 5), seq(4, 79, by = 5), sep = "-"), "80+")
  as.character(cut(age, breaks = breaks, labels = labels, right = FALSE))
}

aggregate_microdata_age <- function(data, geography_labels, measure, spec) {
  if (is.null(spec$age_column) || !spec$age_column %in% names(data)) return(NULL)
  age <- decode_source_age(data, spec)
  age_group <- age_group_quinquennial(age)
  combined <- paste(geography_labels, age_group, sep = "\r")
  valid <- !is.na(geography_labels) & !is.na(age_group)
  if (!any(valid)) return(NULL)
  result <- aggregate_microdata_dimension(
    data[valid, , drop = FALSE], combined[valid], measure, "age", "Faixa etária"
  )
  pieces <- strsplit(result$label, "\r", fixed = TRUE)
  result$label <- vapply(pieces, `[[`, character(1), 1L)
  result$age_group <- vapply(pieces, `[[`, character(1), 2L)
  result
}

aggregate_microdata_age_series <- function(data, period_labels, measure, spec) {
  if (is.null(spec$age_column) || !spec$age_column %in% names(data)) return(NULL)
  age <- decode_source_age(data, spec)
  age_group <- age_group_quinquennial(age)
  combined <- paste(period_labels, age_group, sep = "\r")
  valid <- !is.na(period_labels) & !is.na(age_group)
  if (!any(valid)) return(NULL)
  result <- aggregate_microdata_dimension(
    data[valid, , drop = FALSE], combined[valid], measure, "age_period", "Faixa etária"
  )
  pieces <- strsplit(result$label, "\r", fixed = TRUE)
  result$label <- vapply(pieces, `[[`, character(1), 1L)
  result$age_group <- vapply(pieces, `[[`, character(1), 2L)
  result
}

aggregate_microdata_bundle <- function(
  data,
  query,
  spec = microdata_source_spec(query$domain, query$dataset),
  refresh = FALSE,
  complete_dimensions = TRUE
) {
  parts <- microdata_period_parts(query)
  period_key <- microdata_record_period_key(data, spec)
  selected <- period_key %in% parts$key
  data <- data[!is.na(selected) & selected, , drop = FALSE]
  period_key <- period_key[!is.na(selected) & selected]
  data <- apply_microdata_filters(data, query, spec)
  period_key <- microdata_record_period_key(data, spec)
  measure <- microdata_measure_spec(spec, query$measure_value)

  map_periods <- if (query$frequency == "snapshot") latest_period_row(query$periods) else query$periods
  map_query <- query
  map_query$periods <- map_periods
  map_keys <- microdata_period_parts(map_query)$key
  map_rows <- period_key %in% map_keys
  map_source <- data[!is.na(map_rows) & map_rows, , drop = FALSE]

  geography_labels <- microdata_geography_labels(map_source, query, spec, refresh)
  invalid_geographies <- sum(is.na(geography_labels))
  map <- aggregate_microdata_dimension(
    map_source, geography_labels, measure, "geography", pretty_filter_name(spec$geo_column)
  )
  age_events <- if (isTRUE(measure$standardizable[[1L]])) {
    aggregate_microdata_age(map_source, geography_labels, measure, spec)
  } else {
    NULL
  }
  age_missing_records <- if (!is.null(spec$age_column) && spec$age_column %in% names(map_source)) {
    sum(is.na(decode_source_age(map_source, spec)))
  } else {
    NA_real_
  }

  matching_filters <- names(spec$filters)[
    vapply(spec$filters, function(item) identical(item$column, spec$ranking$column), logical(1))
  ]
  ranking_definition <- if (length(matching_filters) > 0L) {
    spec$filters[[matching_filters[[1L]]]]
  } else {
    NULL
  }
  fixed_choices <- if (!is.null(ranking_definition)) ranking_definition$choices %||% NULL else NULL
  if (identical(measure$measure_type[[1L]], "proportion")) {
    ranking <- map
    ranking$dimension <- "ranking"
    ranking_kind <- "territory"
  } else {
    ranking_labels <- microdata_lookup_labels(
      map_source[[spec$ranking$column]], spec$ranking$lookup, fixed_choices
    )
    ranking <- aggregate_microdata_dimension(
      map_source, ranking_labels, measure, "ranking", spec$ranking$label
    )
    ranking_kind <- "condition"
  }
  warnings <- if (invalid_geographies > 0L) {
    paste0(invalid_geographies, " registro(s) ficaram sem território identificável no mapa.")
  } else {
    character()
  }
  if (nrow(ranking) == 0L) {
    ranking <- map
    ranking$dimension <- "ranking"
    ranking_kind <- "territory"
    warnings <- "A dimensão de ranking não continha valores; o ranking mostra territórios."
  }

  period_labels <- parts$label[match(period_key, parts$key)]
  series <- aggregate_microdata_dimension(data, period_labels, measure, "period", "Período")
  age_series <- if (isTRUE(measure$standardizable[[1L]])) {
    aggregate_microdata_age_series(data, period_labels, measure, spec)
  } else {
    NULL
  }
  if (query$frequency == "snapshot" && nrow(query$periods) > 1L) {
    warnings <- c(
      warnings,
      "O mapa e o ranking do CNES usam a última competência; a série preserva todos os estoques."
    )
  }

  total_details <- reduce_measure_details(map_source, measure)
  bundle <- list(
    map = map,
    total = total_details$value,
    ranking = ranking,
    ranking_kind = ranking_kind,
    series = series,
    age_events = age_events,
    age_series = age_series,
    measure = as.list(measure[1L, , drop = FALSE]),
    quality = list(
      numerator_records = total_details$numerator,
      eligible_records = total_details$eligible,
      missing_measure_records = total_details$missing,
      missing_age_records = age_missing_records
    ),
    map_periods = map_periods,
    warnings = unique(warnings),
    provenance = list(),
    retrieved_at = Sys.time()
  )
  if (isTRUE(complete_dimensions)) {
    bundle <- complete_microdata_dimensions(bundle, query, refresh)
  }
  bundle
}

combine_microdata_dimension <- function(tables) {
  data <- dplyr::bind_rows(tables)
  if (nrow(data) == 0L) return(data)
  grouping <- c("dimension", "label", "measure_type", "is_total", "source_line")
  if ("age_group" %in% names(data)) grouping <- c(grouping, "age_group")
  data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(grouping)))
  dplyr::summarise(
    data,
    numerator = sum_or_na(.data$numerator),
    eligible = sum_or_na(.data$eligible),
    missing = sum(.data$missing, na.rm = TRUE),
    value = if (dplyr::first(.data$measure_type) == "proportion") {
      if (is.na(.data$eligible[[1L]]) || .data$eligible[[1L]] <= 0) NA_real_ else
        .data$numerator[[1L]] / .data$eligible[[1L]] * 100
    } else if (any(is.na(.data$value))) {
      NA_real_
    } else {
      sum(.data$value)
    },
    .groups = "drop"
  )
}

combine_microdata_bundles <- function(bundles) {
  totals <- vapply(bundles, `[[`, numeric(1), "total")
  measure_type <- bundles[[1L]]$measure$measure_type[[1L]]
  eligible <- sum(vapply(bundles, function(item) item$quality$eligible_records, numeric(1)))
  numerator <- sum(vapply(bundles, function(item) item$quality$numerator_records, numeric(1)), na.rm = TRUE)
  list(
    map = combine_microdata_dimension(lapply(bundles, `[[`, "map")),
    total = if (measure_type == "proportion") {
      if (eligible <= 0) NA_real_ else numerator / eligible * 100
    } else if (any(is.na(totals))) {
      NA_real_
    } else {
      sum(totals)
    },
    ranking = combine_microdata_dimension(lapply(bundles, `[[`, "ranking")),
    ranking_kind = if (all(vapply(bundles, `[[`, character(1), "ranking_kind") == "condition")) {
      "condition"
    } else {
      "territory"
    },
    series = combine_microdata_dimension(lapply(bundles, `[[`, "series")),
    age_events = combine_microdata_dimension(lapply(bundles, `[[`, "age_events")),
    age_series = combine_microdata_dimension(lapply(bundles, `[[`, "age_series")),
    measure = bundles[[1L]]$measure,
    quality = list(
      numerator_records = numerator,
      eligible_records = eligible,
      missing_measure_records = sum(vapply(
        bundles, function(item) item$quality$missing_measure_records, numeric(1)
      )),
      missing_age_records = sum(vapply(
        bundles, function(item) item$quality$missing_age_records, numeric(1)
      ), na.rm = TRUE)
    ),
    map_periods = bundles[[1L]]$map_periods,
    warnings = unique(unlist(lapply(bundles, `[[`, "warnings"), use.names = FALSE)),
    provenance = list(),
    retrieved_at = max(do.call(c, lapply(bundles, `[[`, "retrieved_at")))
  )
}

run_microdata_bundle_uncached <- function(query, refresh = FALSE) {
  spec <- microdata_source_spec(query$domain, query$dataset)
  parts <- microdata_period_parts(query)
  ufs <- microdata_query_ufs(query, spec)
  bundles <- vector("list", length(ufs))
  acquisitions <- vector("list", length(ufs))
  fetch_warnings <- character()

  for (index in seq_along(ufs)) {
    uf <- if (is.na(ufs[[index]])) NULL else ufs[[index]]
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::incProgress(
        0.45 / length(ufs),
        detail = paste0("Arquivo ", index, "/", length(ufs), " · ", uf %||% "Brasil")
      )
    }
    started_at <- Sys.time()
    fetched <- fetch_microdata_slice(spec, parts, uf, refresh, query)
    elapsed_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    bundles[[index]] <- aggregate_microdata_bundle(
      fetched$data, query, spec, refresh, complete_dimensions = FALSE
    )
    acquisitions[[index]] <- data.frame(
      uf = uf %||% "BR",
      provider = fetched$provider,
      records = nrow(fetched$data),
      files = paste(fetched$source_files, collapse = ", "),
      urls = paste(stats::na.omit(fetched$source_manifest$url), collapse = ", "),
      releases = paste(stats::na.omit(fetched$source_manifest$release), collapse = ", "),
      sha256 = paste(stats::na.omit(fetched$source_manifest$sha256), collapse = ", "),
      elapsed_seconds = elapsed_seconds,
      status = "complete",
      stringsAsFactors = FALSE
    )
    fetch_warnings <- c(fetch_warnings, fetched$warnings)
    rm(fetched)
  }

  result <- combine_microdata_bundles(bundles)
  result <- complete_microdata_dimensions(result, query, refresh)
  result$warnings <- unique(c(result$warnings, fetch_warnings))
  result$provenance <- list(
    microdata = list(
      primary_provider = "datasusr",
      fallback_provider = "microdatasus",
      source = spec$source,
      file_type = spec$file_type,
      information_system = spec$information_system,
      packages = list(
        datasusr = safe_package_version("datasusr"),
        microdatasus = safe_package_version("microdatasus")
      ),
      acquisitions = dplyr::bind_rows(acquisitions)
    )
  )
  result
}

microdata_cache_key <- function(query) {
  list(
    provider = "microdata",
    schema_version = CACHE_SCHEMA_VERSION,
    application_version = APP_VERSION,
    datasusr_version = safe_package_version("datasusr"),
    microdatasus_version = safe_package_version("microdatasus"),
    domain = query$domain,
    dataset = query$dataset,
    uf = query$uf,
    geo_level = query$geo_level,
    measure = query$measure_value,
    periods = query$periods,
    filters = query$filters,
    urgent_only = query$urgent_only
  )
}

run_microdata_bundle <- function(query, refresh = FALSE) {
  execution <- cached_call(
    namespace = "microdata-results",
    key = microdata_cache_key(query),
    max_age = 24 * 60 * 60,
    refresh = refresh,
    function_to_run = function() run_microdata_bundle_uncached(query, refresh)
  )
  fallback <- attr(execution, "cache_fallback")
  if (!is.null(fallback)) {
    execution$warnings <- unique(c(
      execution$warnings,
      paste0(
        "Os arquivos DBC estavam indisponíveis; foi usado o cache local de ",
        format(fallback$cached_at, "%d/%m/%Y %H:%M"), "."
      )
    ))
  }
  execution
}

resolve_data_provider <- function(domain, dataset) {
  if (!is_microdata_dataset(domain, dataset)) {
    stop(
      "O conjunto selecionado não possui adaptador DBC auditado: ", domain, "/", dataset, ".",
      call. = FALSE
    )
  }
  "microdata"
}

fetch_datasus_options <- function(domain, dataset, uf = NULL, geo_level = "state", refresh = FALSE) {
  resolve_data_provider(domain, dataset)
  fetch_microdata_options(domain, dataset, uf, geo_level, refresh)
}

source_filter_choices <- function(options, domain, dataset, field, uf = NULL) {
  if (is.null(field) || !nzchar(field)) return(NULL)
  definition <- microdata_source_spec(domain, dataset)$filters[[field]]
  if (is.null(definition)) {
    stop("Filtro sem adaptador de microdados: ", field, ".", call. = FALSE)
  }
  if (identical(uf, "all") || identical(uf, "")) uf <- NULL
  microdata_filter_choices(definition, uf)
}

resolve_urgency_filter <- function(options) {
  filter_index <- classify_source_filters(options)
  urgency <- filter_index[filter_index$role == "urgency", , drop = FALSE]
  if (nrow(urgency) == 0L) return(NULL)
  field <- urgency$field[[1L]]
  selected <- resolve_option(options$filtros[[field]], c("Urgência", "Urgente", "Emergência"))
  if (is.null(selected)) return(NULL)
  list(field = field, value = selected$value[[1L]])
}

run_datasus_bundle <- function(query, refresh = FALSE) {
  if (!identical(query$provider, "microdata")) {
    stop("Somente adaptadores DBC auditados são aceitos no runtime.", call. = FALSE)
  }
  run_microdata_bundle(query, refresh)
}
