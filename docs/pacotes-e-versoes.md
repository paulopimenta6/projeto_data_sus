# Auditoria de pacotes e referências

Verificação realizada em 30 de agosto de 2026.

| Referência | Situação verificada | Decisão do projeto |
|---|---|---|
| `datasus` 0.4.1.1 no RDocumentation | Versão publicada em 2019; removida do CRAN em 21/11/2020 e disponível apenas no arquivo | Não instalar a versão antiga |
| `rpradosiqueira/datasus` | Versão marcada `v0.16.1`, em desenvolvimento ativo e fora do CRAN | Fixar tag `v0.16.1` e commit `eec28e81` no `renv.lock` |
| `datasusr` | CRAN 0.1.0, publicado em 04/05/2026 | Usar como camada DBC de baixo nível e dependência do `datasus` |
| `microdatasus` | CRAN 3.0.0, publicado em 29/07/2026 | Usar para processamento e tabelas auxiliares quando microdados forem necessários |
| Artigo Análise Macro | Publicado em 12/01/2022 e baseado em API anterior do `microdatasus` | Referência didática, não contrato de API |
| Guia RPubs `guiamicrodatasus` | Guia comunitário com mais de dois anos | Referência didática secundária |

Dependências espaciais e demográficas verificadas:

| Pacote | Versão | Uso |
|---|---:|---|
| `geobr` | 2.0.1 | Estados, municípios, regiões de saúde e estabelecimentos |
| `sidrar` | 0.5.0 | População municipal após 2021 |
| `leaflet` | 2.2.3 | Mapas interativos |
| `renv` | 1.2.4 | Ambiente reproduzível |

## Motivo da combinação

`datasus` 0.16.1 oferece consultas agregadas e descoberta de filtros adequadas a um painel nacional. `datasusr` é mais eficiente para leitura DBC e cache de arquivos brutos, mas não fornece rótulos clínicos. `microdatasus` possui processadores por sistema, mas baixar todos os microdados nacionais do SIA ou SIH para cada interação seria inadequado.

O painel usa TABNET como caminho principal e mantém as outras bibliotecas declaradas para extensões de detalhe. Essa separação reduz memória, tempo de resposta e risco de travamento local.

## Fontes consultadas

- <https://www.rdocumentation.org/packages/datasus/versions/0.4.1.1>
- <https://cran.r-project.org/package=datasus>
- <https://github.com/rpradosiqueira/datasus>
- <https://cran.r-project.org/package=datasusr>
- <https://github.com/StrategicProjects/datasusr>
- <https://cran.r-project.org/package=microdatasus>
- <https://github.com/rfsaldanha/microdatasus>
- <https://analisemacro.com.br/data-science/dicas-de-rstats/hackeando-o-r-acessando-os-dados-do-datasus-com-o-r/>
- <https://rpubs.com/romulofreits/guiamicrodatasus>
