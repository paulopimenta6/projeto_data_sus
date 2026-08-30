# Pacotes e versões

Esta é uma referência técnica. Quem deseja apenas usar o painel pode executar `Rscript prepare_environment.R` e não precisa instalar os pacotes desta página um por um.

## Como o ambiente é mantido

O projeto usa dois arquivos principais:

- `DESCRIPTION` declara os pacotes necessários.
- `renv.lock` registra as versões exatas e a origem de cada pacote.

O `prepare_environment.R` lê esses arquivos, instala bibliotecas do Ubuntu/Debian, restaura o ambiente e confere as versões. Executar `install.packages()` manualmente não é recomendado porque pode criar combinações diferentes das que foram testadas.

## Pacotes principais

| Pacote | Versão registrada | Função no projeto |
|---|---:|---|
| `datasus` | 0.16.1 | Consulta agregada e descoberta de filtros TABNET |
| `datasusr` | 0.1.0 | Leitura DBC e suporte de baixo nível |
| `microdatasus` | 3.0.0 | Processamento de microdados em extensões futuras |
| `geobr` | 2.0.1 | Limites territoriais e estabelecimentos |
| `sidrar` | 0.5.0 | População municipal do IBGE/SIDRA |
| `leaflet` | 2.2.3 | Mapas interativos |
| `renv` | 1.2.4 | Biblioteca isolada e reprodução de versões |
| `pak` | 0.11.1 | Descoberta de bibliotecas do sistema |

O `renv.lock` é a fonte definitiva caso a tabela acima fique desatualizada.

## Por que o pacote datasus vem do GitHub

A versão antiga `datasus` 0.4.1.1 foi removida do CRAN em 2020. O projeto usa `rpradosiqueira/datasus` na tag `v0.16.1`, fixada no commit:

```text
eec28e81ef833d344b3c6a170de7456945e87360
```

Fixar o commit impede que uma mudança futura no repositório altere silenciosamente o comportamento da aplicação.

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

Verificação original realizada em 30 de agosto de 2026.

| Referência | Situação observada |
|---|---|
| `datasus` no RDocumentation | Versão 0.4.1.1, publicada em 2019 e arquivada |
| `rpradosiqueira/datasus` | Tag `v0.16.1`, usada pelo projeto |
| `datasusr` | Pacote CRAN para leitura e cache DBC |
| `microdatasus` | Pacote CRAN com processadores por sistema |

Fontes consultadas:

- <https://github.com/rpradosiqueira/datasus>
- <https://cran.r-project.org/package=datasusr>
- <https://cran.r-project.org/package=microdatasus>
- <https://ipeagit.github.io/geobr/>
- <https://rstudio.github.io/renv/>
- <https://pak.r-lib.org/>
