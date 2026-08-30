# Entenda a metodologia

Esta página explica como o painel transforma uma escolha feita na tela em um resultado. Os detalhes ajudam a interpretar os números, mas não são necessários para começar a usar o sistema.

## O que o painel consulta

O painel usa tabelas agregadas do TABNET. Em vez de baixar milhões de registros individuais, ele pede ao DATASUS uma tabela já resumida para os filtros escolhidos.

Uma análise pode realizar três consultas:

1. Uma consulta por território para a tabela e o mapa.
2. Uma consulta por condição, procedimento ou categoria para o ranking.
3. Uma consulta por mês ou ano para a série temporal.

Se uma dimensão não estiver disponível, o painel usa uma alternativa e mostra um aviso. Por exemplo, o ranking pode passar a mostrar territórios.

## O que é um total

Total é a soma da medida escolhida no período e no recorte selecionados. O significado depende da fonte:

- no SIM, pode ser número de óbitos;
- no SIH, pode ser número de internações ou AIH;
- no SIA, pode ser quantidade aprovada;
- no SINAN, pode ser número de notificações;
- no CNES, pode ser número de estabelecimentos ou recursos cadastrados.

Linhas chamadas “Total” pelo TABNET são identificadas antes da soma para evitar dupla contagem.

## O que é uma taxa por 100 mil

A taxa bruta é calculada assim:

```text
eventos ÷ população × 100.000
```

Exemplo: 50 eventos em uma população de 100.000 habitantes correspondem a uma taxa de 50 por 100 mil.

Taxas são úteis para comparar populações de tamanhos diferentes. Elas não corrigem diferenças de idade, sexo ou composição social; por isso são chamadas de taxas brutas.

Medidas monetárias, médias, percentuais e taxas que já vêm prontas da fonte não recebem uma nova taxa.

## Como meses e anos entram no denominador

Para um ano completo, o painel usa a população daquele ano. Quando vários anos são somados, as populações anuais formam pessoas-ano.

Quando apenas alguns meses são somados, o denominador geral usa a fração correspondente do ano. Uma seleção de três meses, por exemplo, usa `3/12` da população anual como pessoas-ano.

Na série mensal, cada ponto usa a população do ano sem anualizar o número de eventos.

## Intervalo de confiança

Quando o numerador é uma contagem inteira, o painel calcula um intervalo exato de Poisson de 95%.

O intervalo mostra a incerteza estatística em torno da taxa. Intervalos largos são comuns quando há poucos eventos e devem levar a uma interpretação mais cautelosa.

## Como os territórios são ligados ao mapa

Códigos municipais de seis dígitos do DATASUS são convertidos para códigos IBGE de sete dígitos. O código é a primeira tentativa de ligação com a geometria.

Se o código não produzir correspondência, o painel tenta o nome normalizado do território. Linhas que continuarem sem geometria aparecem em um aviso e permanecem na tabela.

O sistema usa SIRGAS 2000 e transforma o resultado para o formato exigido pelo mapa interativo.

## Particularidade do CNES

CNES representa estoque cadastrado, não eventos. Quando mais de uma competência é selecionada:

- a série preserva cada competência;
- o mapa e o ranking usam a competência mais recente;
- estabelecimentos não são somados entre meses.

Pontos inativos, coordenadas não numéricas e coordenadas fora dos limites aproximados do Brasil são retirados do mapa de estabelecimentos.

## De onde vem cada população

| Ano | Regra |
|---|---|
| Até 2021 | Estimativa municipal do DATASUS |
| 2022 | Censo 2022, SIDRA tabela 4709, variável 93 |
| 2023 | Sem interpolação automática; taxa ausente quando não há denominador |
| 2024 em diante | SIDRA tabela 6579, variável 9324 |

Se faltar população para qualquer ano necessário em uma análise multiperíodo, o denominador daquele território fica ausente. O painel não inventa ou interpola valores silenciosamente.

## Como reproduzir um resultado

O download **Manifesto · JSON** registra:

- fonte e conjunto de dados;
- medida e períodos;
- filtros enviados ao TABNET;
- data e horário da consulta;
- ano dos limites territoriais;
- versões do R e dos principais pacotes;
- avisos metodológicos.

Guarde o manifesto junto com as tabelas e figuras quando precisar documentar uma análise.

## Glossário rápido

| Termo | Significado neste projeto |
|---|---|
| AIH | Autorização de Internação Hospitalar |
| Competência | Mês ou período administrativo de registro |
| Denominador | População usada para calcular uma taxa |
| GeoJSON | Arquivo de dados geográficos que pode ser aberto em programas de mapas |
| Pessoas-ano | População multiplicada pelo tempo observado |
| TABNET | Ferramenta pública de tabelas do DATASUS |

## Cuidado na interpretação

Os resultados descrevem registros administrativos. Diferenças entre lugares podem refletir ocorrência de doenças, tamanho da população, acesso aos serviços, encaminhamento, regras de financiamento e qualidade de preenchimento.

Uma associação no mapa ou no tempo não prova causa. Para decisões clínicas, epidemiológicas ou de gestão, combine os resultados com conhecimento local e notas técnicas oficiais.
