# Projeto Data SUS

Aplicação Shiny local para consultar, resumir e mapear dados públicos do Sistema Único de Saúde. O painel permite selecionar condição, procedimento ou código CID, período e recorte geográfico, produzindo totais, taxas, séries temporais, rankings, mapas coropléticos e pontos de estabelecimentos.

## Escopo

| Tema | Sistema | Exemplos de resultados |
|---|---|---|
| Mortalidade | SIM | Óbitos por causa básica, residência ou ocorrência |
| Morbidade hospitalar | SIH | Internações, diagnósticos, óbitos e permanência |
| Produção hospitalar | SIH | AIH, procedimentos, valores e urgências |
| Produção ambulatorial | SIA | Quantidade aprovada, procedimentos e valores |
| Agravos de notificação | SINAN | Casos e classificações disponíveis no TABNET |
| Estrutura assistencial | CNES | Estabelecimentos, leitos, profissionais e equipamentos |

Neste projeto, **demanda** significa utilização ou produção registrada no SUS. O painel não estima filas, necessidade reprimida ou demanda não atendida.

## Início rápido

Pré-requisitos:

- R 4.1 ou superior;
- Git;
- acesso HTTPS aos servidores do DATASUS, IBGE/SIDRA e repositório de dados do `geobr`;
- bibliotecas de sistema exigidas por `sf`, Arrow e DuckDB.

Em Ubuntu/Debian, a instalação típica das bibliotecas de compilação é:

```bash
sudo apt install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev \
  libgdal-dev libgeos-dev libproj-dev libudunits2-dev libabsl-dev pandoc
```

DuckDB pode levar vários minutos para compilar quando não houver binário compatível.

Na raiz do repositório:

```bash
Rscript scripts/setup.R
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

O primeiro comando restaura exatamente as dependências registradas em `renv.lock`. O primeiro acesso a uma fonte ou geometria pode demorar; as respostas seguintes usam o cache local em `data/cache/`.

## Uso

1. Selecione o tema e o conjunto de dados.
2. Escolha Brasil ou uma UF e o nível geográfico do mapa.
3. Clique em **Carregar opções da fonte**. Medidas, períodos e filtros são lidos do formulário TABNET atual.
4. Selecione uma medida, um ou mais períodos e filtros opcionais de condição e território.
5. Escolha total ou taxa por 100 mil e clique em **Analisar**.
6. Consulte a visão geral, os mapas e a tabela territorial.
7. Exporte CSV, GeoJSON, PNG, HTML ou o manifesto JSON da consulta.

Fontes mensais selecionam inicialmente a penúltima competência publicada, pois a mais recente pode ainda não conter registros. Todas as competências continuam disponíveis para escolha manual.

Para o mapa de pontos do CNES, selecione uma UF na consulta principal. Essa proteção evita carregar no navegador o arquivo nacional com mais de 600 mil estabelecimentos.

## Resultados e exportação

O painel oferece:

- indicadores de total, taxa geral, maior valor territorial e cobertura geográfica;
- série temporal na periodicidade disponibilizada pela fonte;
- ranking de condições, procedimentos ou categorias, com fallback explícito para territórios;
- mapa coroplético interativo;
- mapa de pontos de hospitais, urgências, UBS ou todos os estabelecimentos;
- tabela pesquisável;
- exportação de territórios, série e ranking em CSV;
- mapas em GeoJSON, PNG e HTML;
- pontos CNES em GeoJSON;
- manifesto JSON com filtros, versões, fonte, data de consulta e avisos.

## Metodologia

Consultas nacionais usam tabelas agregadas do TABNET. Essa estratégia evita carregar todos os microdados do SIH ou SIA em uma sessão Shiny. `microdatasus` e `datasusr` permanecem disponíveis para leitura seletiva e processamento de layouts DBC quando uma análise futura exigir registros individuais.

Os resultados TABNET são normalizados imediatamente, porque nomes de colunas e filtros são definidos pelo formulário ativo e podem mudar. O projeto mantém os valores brutos utilizados na requisição e acrescenta um manifesto de proveniência mais completo que o atributo padrão do pacote `datasus`.

Consulte:

- [`docs/metodologia.md`](docs/metodologia.md)
- [`docs/fontes-de-dados.md`](docs/fontes-de-dados.md)
- [`docs/pacotes-e-versoes.md`](docs/pacotes-e-versoes.md)

## Taxas e população

- Até 2021: estimativas municipais publicadas no DATASUS.
- 2022: Censo Demográfico, SIDRA tabela 4709, variável 93.
- 2023: taxa omitida quando não houver denominador municipal oficial compatível.
- A partir de 2024: estimativas municipais, SIDRA tabela 6579, variável 9324.
- Intervalos de 95% usam o método exato de Poisson quando o numerador é uma contagem inteira.
- Agregações mensais de vários meses usam pessoas-ano no denominador geral.

## Cache

Por padrão, o cache fica em `data/cache/` e não é versionado. Para usar outro diretório:

```bash
export PROJETO_DATASUS_CACHE_DIR="/caminho/para/cache"
```

Respostas de formulários são mantidas por 6 horas, resultados TABNET por 7 dias, geometrias por 1 ano e estabelecimentos CNES por 30 dias.

## Testes

Verificação offline completa:

```bash
Rscript scripts/check.R
```

Teste opcional contra o formulário ativo do DATASUS:

```bash
RUN_LIVE_DATASUS_TESTS=true Rscript tests/testthat.R
```

Indisponibilidade ou lentidão externa marca o teste ao vivo como ignorado; respostas recebidas com contrato incompatível continuam falhando.

## Estrutura

```text
R/02_catalog.R         catálogo e classificação de opções
R/03_query.R           contrato e validação da consulta
R/04_cache.R           cache persistente e atômico
R/05_tabnet.R          adaptador TABNET e normalização
R/06_geography.R       códigos, limites e estabelecimentos
R/07_population.R      denominadores e taxas
R/08_analysis.R        composição dos resultados e manifesto
R/09_visuals.R         gráficos e mapas
R/10_exports.R         formatos de exportação
R/20_mod_*.R           módulos Shiny
tests/testthat/        testes determinísticos e teste ao vivo opcional
```

## Limitações importantes

- SIH registra autorizações de internação, não pacientes únicos.
- SIA registra produção aprovada, não pessoas únicas.
- CNES é um estoque cadastral mensal; competências não são somadas no mapa.
- SINAN possui formulários e critérios específicos para cada agravo.
- Competências recentes podem ser preliminares ou incompletas.
- Mudanças de limites municipais e de regiões de saúde podem afetar séries históricas.
- Taxas apresentadas são brutas e não ajustadas por idade.

## Licença

MIT. Os dados permanecem sujeitos às condições e notas técnicas das fontes oficiais.
