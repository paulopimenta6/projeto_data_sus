# Metodologia

## Unidade de análise

O significado de uma linha varia por sistema. O SIM representa óbitos. O SIH representa AIH e internações registradas conforme a tabela consultada. O SIA representa produção ambulatorial apresentada ou aprovada. O SINAN representa notificações e investigações. O CNES representa estabelecimentos e recursos cadastrados em uma competência.

O painel preserva essas diferenças e não usa o termo paciente para unidades administrativas que podem incluir repetições da mesma pessoa.

## Estratégia de aquisição

O adaptador consulta primeiro `datasus::datasus_opcoes()`. A resposta contém dimensões de linha e coluna, medidas, competências e filtros publicados no formulário naquele momento. A interface usa o rótulo para apresentação e o valor bruto para a requisição.

Cada análise pode executar até três consultas agregadas:

1. território na linha, para tabela e mapa;
2. condição, procedimento ou categoria na linha, para ranking;
3. competência ou ano na linha, para série temporal.

Quando não existe dimensão temporal, o painel consulta cada competência separadamente. Quando não existe dimensão de condição, o ranking territorial é usado e um aviso é exibido.

Consultas de múltiplas competências são agregadas apenas quando a finalidade é explicitamente o total do período. O manifesto registra todos os valores enviados ao TABNET.

## CNES

CNES contém fotografias cadastrais mensais, não eventos. Se várias competências forem selecionadas:

- a série mostra cada competência;
- o mapa e o ranking usam a competência mais recente;
- nenhuma soma de estabelecimentos entre meses é produzida.

Pontos de estabelecimentos vêm de `geobr::read_health_facilities()`. Registros com motivo de desativação, coordenadas não finitas ou coordenadas fora dos limites aproximados do Brasil são excluídos do mapa. O painel não corrige silenciosamente coordenadas inválidas.

## Geografia

Códigos municipais DATASUS de seis dígitos são convertidos para códigos IBGE de sete dígitos com `datasus::normalizar_codigo_ibge()`. Limites vêm do `geobr` e usam SIRGAS 2000.

O ano da geometria é o mais recente que não ultrapassa o ano analítico, entre os anos disponíveis. Quando a fonte não possui a mesma safra territorial, o ano efetivamente usado aparece na tela e no manifesto.

Regiões de saúde mudam ao longo do tempo. A versão de 2022, por exemplo, usa a referência disponível anterior; não se deve interpretar essa associação como vigência jurídica exata.

## Totais e séries

Colunas numéricas múltiplas retornadas pelo TABNET são somadas para o total solicitado. Linhas `Total` são identificadas e removidas antes de agregações por território para impedir dupla contagem.

Em fontes mensais, subtotais anuais presentes na mesma dimensão são removidos quando competências mensais específicas foram selecionadas.

## Taxas

A taxa bruta é:

```text
eventos / denominador × 100.000
```

Para um mapa que agrega parte de um ano, o denominador é convertido em pessoas-ano pela fração de competências selecionadas. Para uma série mensal, cada ponto é expresso por 100 mil residentes usando a população anual, sem anualização do numerador.

Para períodos anuais completos, soma-se a população de cada ano, formando pessoas-ano. Taxas não são calculadas para medidas monetárias, médias, proporções ou taxas já fornecidas pela fonte.

O intervalo exato de Poisson de 95% é calculado apenas quando numerador e denominador são válidos e o numerador é uma contagem inteira não negativa.

## Denominadores

| Ano | Fonte |
|---|---|
| Até 2021 | DATASUS, estimativa municipal |
| 2022 | SIDRA 4709, variável 93, Censo 2022 |
| 2023 | Sem estimativa na SIDRA 6579; resultado fica ausente |
| 2024 em diante | SIDRA 6579, variável 9324 |

Ausência de qualquer ano exigido invalida o denominador do território para o total multiperíodo. O painel não usa interpolação automática.

## Proveniência

O manifesto JSON inclui:

- domínio, conjunto e função de origem;
- UF e nível geográfico;
- rótulo e valor bruto da medida;
- rótulos e valores brutos das competências;
- filtros enviados;
- data da consulta;
- URL e dados de proveniência expostos pelo pacote;
- ano da geometria;
- versões de R e dos principais pacotes;
- avisos metodológicos.

## Interpretação

Os resultados descrevem registros administrativos públicos. Diferenças territoriais podem refletir cobertura, organização da rede, acesso, regras de financiamento, qualidade de preenchimento e fluxo de pacientes, além da ocorrência epidemiológica.
