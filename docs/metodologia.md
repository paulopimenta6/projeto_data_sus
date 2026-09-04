# Entenda a metodologia

Esta página explica como o painel transforma uma escolha feita na tela em um resultado. Os detalhes ajudam a interpretar os números, mas não são necessários para começar a usar o sistema.

Para aprender primeiro pelos botões e por um exemplo, use o
[Guia de primeiros passos](guia-do-usuario.md).

## O que o painel consulta

O painel usa os arquivos DBC públicos do DATASUS. Um registro local define, para cada conjunto, quais colunas representam período, território, medida, condição e ranking. Assim, a interface não depende da estrutura variável de formulários web.

O pacote `datasusr` lista e lê os arquivos como rota principal. Se essa rota falhar, `microdatasus` tenta o mesmo recorte. Os arquivos completos ficam no cache DBC, mas somente as colunas exigidas pela medida, pelo ranking e pelos filtros ativos são carregadas. Filtros de CID, procedimento, ocupação, urgência e território são aplicados antes da agregação.

Listas extensas de municípios, procedimentos e ocupações são carregadas somente quando o filtro correspondente é escolhido. A pesquisa ocorre no servidor e envia ao navegador apenas um pequeno lote de correspondências, em vez de milhares de opções de uma só vez.

Em recortes nacionais publicados por UF, cada estado é processado separadamente para limitar o uso de memória. Quando há filtro municipal, somente as UFs dos municípios selecionados são abertas. A análise só é concluída se a identidade de todos os arquivos esperados para cada UF e período for confirmada e todos eles forem lidos; isso também vale para a rota de contingência. Falhas não produzem resultados nacionais parciais. Por esse motivo, o número máximo de períodos depende do sistema e da abrangência.

Uma análise pode realizar três consultas:

1. Uma agregação por território para a tabela e o mapa.
2. Uma agregação por condição, procedimento ou categoria para o ranking.
3. Uma agregação por mês ou ano para a série temporal.

Se uma dimensão não estiver disponível, o painel usa uma alternativa e mostra um aviso. Por exemplo, o ranking pode passar a mostrar territórios.

Depois que todos os arquivos são confirmados, competências sem registros compatíveis e territórios elegíveis sem eventos aparecem com valor zero. Uma falha de aquisição ou um arquivo ausente nunca é convertido em zero.

## O que é um total

Total é a soma da medida escolhida no período e no recorte selecionados. O significado depende da fonte:

- no SIM, pode ser número de óbitos;
- no SIH, pode ser número de internações ou AIH;
- no SIA, pode ser quantidade aprovada;
- no SINAN, pode ser número de notificações;
- no SINASC, pode ser número de nascidos vivos ou proporção entre registros elegíveis;
- no CNES, pode ser número de estabelecimentos ou recursos cadastrados.

Cada sistema possui uma redução explícita. SIH e SIM contam registros quando a medida é uma ocorrência; SIA soma quantidades ou valores aprovados/apresentados; CNES conta estabelecimentos distintos ou soma leitos. O total é calculado diretamente sobre o recorte filtrado, sem somar linhas de subtotal.

## Qual território cada sistema representa

O mapa usa uma coluna territorial fixa e documentada por adaptador:

- SIM pode usar residência ou ocorrência; SINAN usa residência;
- SIH pode usar residência ou processamento/internação (`MUNIC_MOV`);
- SIA usa município do estabelecimento (`PA_UFMUN`);
- CNES usa município do estabelecimento (`CODUFMUN`).
- SINASC pode usar residência da mãe ou ocorrência do nascimento.

Essas escolhas evitam misturar residência com local de atendimento. Para SIH e SIA, uma UF selecionada representa os arquivos processados naquela UF; isso não equivale a medir todos os residentes da UF quando existe atendimento interestadual.

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

Na série mensal, cada ponto usa `1/12` da população anual. Isso transforma o resultado em uma taxa anualizada por 100 mil pessoas-ano e mantém a mesma unidade usada no mapa do período. Essa taxa ajuda a comparar a velocidade de ocorrência entre meses; para descrever a carga observada em um mês, consulte também o total.

## Intervalo de confiança

Quando o numerador é uma contagem inteira, o painel calcula um intervalo exato de Poisson de 95%.

O intervalo mostra a incerteza estatística em torno da taxa. Intervalos largos são comuns quando há poucos eventos e devem levar a uma interpretação mais cautelosa.

## Padronização direta por idade

Medidas elegíveis podem usar faixas `0–4`, `5–9`, até `75–79` e `80+`, com a
população brasileira do Censo 2010 como padrão. O IC95% segue o método gama de
Fay–Feuer. A taxa não é calculada se idade, numerador ou denominador estiver incompleto;
ela também não corrige sub-registro, acesso ou qualidade do preenchimento.

No SIH, `IDADE` é interpretada em conjunto com `COD_IDADE`: dias e meses entram em
`0–4`, anos são usados diretamente e o código de centena representa `100 + idade`.
Atualmente, os denominadores municipais por faixa etária estão configurados somente
para 2010. Em outros anos, o painel informa a limitação e volta à taxa bruta quando ela
estiver disponível, sem reutilizar silenciosamente a população etária de 2010.

## Comparações e insights

Municípios e regiões são comparados com o total compatível da UF; consultas nacionais
usam o Brasil. As regras mostram mudança frente ao período anterior, razão frente à
referência e concentração territorial, sempre com fórmula e ressalva descritiva.

## Como os territórios são ligados ao mapa

Códigos municipais de seis dígitos do DATASUS são convertidos para códigos IBGE de sete dígitos. O código é a primeira tentativa de ligação com a geometria.

Se o código não produzir correspondência, o painel tenta o nome normalizado do território. Linhas que continuarem sem geometria aparecem em um aviso e permanecem na tabela.

O sistema usa SIRGAS 2000 e transforma o resultado para o formato exigido pelo mapa interativo.

Em análises por região de saúde, numeradores, denominadores e mapa usam uma única referência territorial: o limite disponível mais próximo, sem ultrapassar o ano mais recente da consulta. Isso evita combinar composições regionais de anos diferentes na mesma taxa.

## Particularidade do CNES

CNES representa estoque cadastrado, não eventos. Quando mais de uma competência é selecionada:

- a série preserva cada competência;
- o mapa e o ranking usam a competência mais recente;
- estabelecimentos não são somados entre meses.

Pontos inativos, coordenadas não numéricas e coordenadas fora dos limites aproximados do Brasil são retirados do mapa de estabelecimentos.

## De onde vem cada população

| Ano | Regra |
|---|---|
| 1996-1999 | Sem denominador municipal SIDRA configurado; taxa ausente |
| 2000 | Censo 2000, SIDRA tabela 202, variável 93 |
| 2001-2006, 2008-2009 e 2011-2021 | SIDRA tabela 6579, variável 9324 |
| 2010 | Censo 2010, SIDRA tabela 608, variável 93 |
| 2022 | Censo 2022, SIDRA tabela 4709, variável 93 |
| 2007 e 2023 | Sem interpolação automática; taxa ausente |
| 2024 em diante | SIDRA tabela 6579, variável 9324 |

Se faltar população para qualquer território ou ano necessário, o denominador correspondente fica ausente. Totais estaduais e regionais também ficam ausentes quando algum componente conhecido não possui população. Filtros municipais são aplicados antes dessas agregações, e territórios com zero eventos continuam no denominador. O painel não soma denominadores parciais nem inventa ou interpola valores silenciosamente.

## Como reproduzir um resultado

O download **Manifesto · JSON** registra:

- fonte e conjunto de dados;
- medida e períodos;
- filtros aplicados localmente aos microdados;
- data e horário da consulta;
- arquivos, URLs, tipo de publicação e checksum SHA-256 disponíveis na aquisição;
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
| DBC | Formato comprimido usado nos arquivos públicos do DATASUS |
| SIGTAP | Tabela de procedimentos, medicamentos e OPM do SUS |

## Cuidado na interpretação

Os resultados descrevem registros administrativos. Diferenças entre lugares podem refletir ocorrência de doenças, tamanho da população, acesso aos serviços, encaminhamento, regras de financiamento e qualidade de preenchimento.

Uma associação no mapa ou no tempo não prova causa. Para decisões clínicas, epidemiológicas ou de gestão, combine os resultados com conhecimento local e notas técnicas oficiais.
