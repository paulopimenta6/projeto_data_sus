# Fontes de dados

## SIM

Sistema de Informação sobre Mortalidade. A unidade básica é o óbito registrado na Declaração de Óbito. O painel pode usar causa básica CID-10, residência ou ocorrência conforme a medida e o conjunto selecionados.

Periodicidade dos arquivos: anual.

## SIH/SUS

Sistema de Informações Hospitalares. Reúne autorizações e produção hospitalar financiada pelo SUS. O painel separa morbidade hospitalar de produção de AIH.

Periodicidade: mensal. Local de internação e residência são conjuntos diferentes; o usuário deve escolher o conjunto coerente com a pergunta.

## SIA/SUS

Sistema de Informações Ambulatoriais. Reúne procedimentos apresentados e aprovados por estabelecimento e competência. Quantidade aprovada não equivale a número de pessoas.

Periodicidade: mensal. Filtros de procedimento, grupo, complexidade, caráter de atendimento e tipo de unidade dependem do formulário ativo.

## SINAN

Sistema de Informação de Agravos de Notificação. Cada agravo tem formulário, período, critérios e campos próprios. O catálogo do pacote `datasus` expõe os agravos atualmente ligados pelo TABNET.

O painel não combina automaticamente agravos com definições distintas.

## CNES

Cadastro Nacional de Estabelecimentos de Saúde. Inclui estabelecimentos, leitos, equipamentos, profissionais, serviços e equipes. Cada competência é uma fotografia cadastral.

O mapa de pontos utiliza o conjunto georreferenciado do `geobr`, derivado do CNES e complementado por geocodificação. A coluna de origem da coordenada permanece disponível no GeoJSON exportado.

## População

Denominadores vêm de tabelas oficiais do DATASUS e do IBGE/SIDRA. Códigos são mantidos como texto para preservar zeros e dígitos verificadores.

## Limites territoriais

Estados, municípios, regiões de saúde e pontos CNES vêm do `geobr` 2.0.1. Geometrias simplificadas são usadas na interface para reduzir transferência e renderização; os códigos e o ano permanecem no resultado.

## Disponibilidade externa

DATASUS e SIDRA podem impor limites, alterar formulários ou ficar temporariamente indisponíveis. O cache reduz consultas repetidas, mas nunca converte falha de rede em resultado vazio. Erros e resultados parciais são informados ao usuário.
