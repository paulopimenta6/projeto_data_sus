# Pacotes e versões

Esta é uma referência técnica. Quem deseja apenas usar o painel pode executar `Rscript prepare_environment.R` e não precisa instalar os pacotes desta página um por um.

## Como o ambiente é mantido

O projeto usa dois arquivos principais:

- `DESCRIPTION` declara os pacotes necessários.
- `renv.lock` registra as versões exatas e a origem de cada pacote.

O `prepare_environment.R` lê esses arquivos, instala bibliotecas do Ubuntu/Debian, restaura o ambiente e confere as versões. Executar `install.packages()` manualmente não é recomendado porque pode criar combinações diferentes das que foram testadas.

## Pacotes principais

| Pacote | Versão registrada | Publicação verificada | Função no projeto |
|---|---:|---:|---|
| `datasus` | 0.16.1 | 05/08/2026 | Consulta agregada e descoberta de filtros TABNET |
| `datasusr` | 0.1.0 | 04/05/2026 | Leitura rápida de DBC disponível no ambiente reproduzível |
| `microdatasus` | 3.0.0 | 29/07/2026 | Processamento de microdados disponível para extensões |
| `geobr` | 2.0.1 | 23/06/2026 | Limites territoriais e estabelecimentos |
| `sidrar` | 0.5.0 | 26/08/2026 | População municipal do IBGE/SIDRA |
| `leaflet` | 2.2.3 | - | Mapas interativos |
| `renv` | 1.2.4 | - | Biblioteca isolada e reprodução de versões |
| `pak` | 0.11.1 | - | Descoberta de bibliotecas do sistema |

O `renv.lock` é a fonte definitiva caso a tabela acima fique desatualizada.

## Por que o pacote datasus vem do GitHub

A versão antiga `datasus` 0.4.1.1 foi removida do CRAN em 2020. O projeto usa `rpradosiqueira/datasus` na tag `v0.16.1`, fixada no commit:

```text
eec28e81ef833d344b3c6a170de7456945e87360
```

Fixar o commit impede que uma mudança futura no repositório altere silenciosamente o comportamento da aplicação. A versão 0.16.1 foi confirmada como a versão estável mais recente do repositório em 31 de agosto de 2026.

## Qual caminho de dados o painel usa

A interface atual usa `datasus` para consultar tabelas agregadas do TABNET. Esse caminho responde rapidamente às escolhas feitas na tela e evita carregar milhões de registros na memória.

Os pacotes `microdatasus` e `datasusr` trabalham com microdados DBC, isto é, registros individuais ou arquivos brutos. Eles estão registrados no ambiente para estudos que precisem desse nível de detalhe, mas a interface atual não baixa microdados nem mistura registros individuais com as tabelas agregadas. Essa separação é intencional e está descrita em `docs/metodologia.md`.

## Compatibilidade do R

O `DESCRIPTION` exige R 4.1 ou mais recente. O lockfile atual foi criado com R 4.6.1.

Quando outra versão compatível é usada, o preparador informa a diferença e tenta restaurar exatamente os pacotes registrados. Se uma versão não puder ser compilada, a instalação para sem substituir silenciosamente o pacote por outro.

## Como atualizar dependências com segurança

Uma atualização deve ser feita por uma pessoa desenvolvedora em uma mudança separada:

1. Atualizar o pacote desejado no ambiente `renv`.
2. Executar `Rscript scripts/check.R`.
3. Executar os testes ao vivo.
4. Testar o painel no navegador.
5. Atualizar `renv.lock` com `renv::snapshot()`.
6. Registrar no Git as alterações do código e do lockfile juntas.

## Auditoria das referências

Verificação atualizada em 31 de agosto de 2026.

| Referência | Situação observada |
|---|---|
| `datasus` no RDocumentation | Versão 0.4.1.1, publicada em 2019; o pacote saiu do CRAN em 2020 |
| `rpradosiqueira/datasus` | Versão estável `v0.16.1`, usada pelo projeto |
| `datasusr` | Versão CRAN 0.1.0 para catálogo, download, cache e leitura DBC |
| `microdatasus` | Versão CRAN 3.0.0; suporta SIM, SINASC, SIH, SIA, CNES e parte do SINAN |
| Guia RPubs do `microdatasus` | Material introdutório; confirme argumentos na documentação atual do pacote |
| Artigo da Análise Macro | Exemplo de 2022; útil como introdução, mas não como fonte de versões atuais |

Fontes consultadas:

- <https://github.com/rpradosiqueira/datasus>
- <https://www.rdocumentation.org/packages/datasus/versions/0.4.1.1>
- <https://cran.r-project.org/package=datasusr>
- <https://cran.r-project.org/package=microdatasus>
- <https://github.com/rfsaldanha/microdatasus>
- <https://rpubs.com/romulofreits/guiamicrodatasus>
- <https://analisemacro.com.br/data-science/dicas-de-rstats/hackeando-o-r-acessando-os-dados-do-datasus-com-o-r/>
- <https://ipeagit.github.io/geobr/>
- <https://rstudio.github.io/renv/>
- <https://pak.r-lib.org/>
