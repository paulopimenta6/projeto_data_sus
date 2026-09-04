# Conheça as fontes de dados

Você não precisa decorar as siglas para usar o painel. Esta página explica, em linguagem direta, o que cada fonte representa e qual cuidado deve ser tomado.

Se esta é sua primeira visita, faça antes o
[roteiro guiado de 10 minutos](guia-do-usuario.md#roteiro-guiado-de-10-minutos).

## Visão rápida

| Sigla | Nome | O que um registro representa | Principal cuidado |
|---|---|---|---|
| SIM | Sistema de Informação sobre Mortalidade | Um óbito registrado | A causa depende do preenchimento da declaração de óbito |
| SIH | Sistema de Informações Hospitalares | Uma autorização ou internação financiada pelo SUS | Não representa necessariamente uma pessoa única |
| SIA | Sistema de Informações Ambulatoriais | Produção ambulatorial apresentada ou aprovada | Quantidade de procedimentos não é quantidade de pessoas |
| SINAN | Sistema de Informação de Agravos de Notificação | Uma notificação ou investigação | Cada doença possui regras próprias |
| CNES | Cadastro Nacional de Estabelecimentos de Saúde | Um estabelecimento ou recurso cadastrado em um mês | É cadastro de estrutura, não atendimento realizado |
| SINASC | Sistema de Informações sobre Nascidos Vivos | Uma declaração de nascido vivo | Campos ignorados alteram o denominador de proporções |

## SIM: mortalidade

O SIM reúne informações das declarações de óbito. Ele permite analisar causas CID-10, ano, residência e local de ocorrência.

Use **residência** para estudar onde viviam as pessoas. Use **ocorrência** para estudar
onde os óbitos aconteceram. Os dois recortes respondem a perguntas diferentes e estão
disponíveis como conjuntos separados no painel.

Os dados costumam ser anuais e podem ser revisados.

## SIH/SUS: internações e produção hospitalar

O SIH registra autorizações de internação hospitalar, conhecidas como AIH. O painel separa duas formas de consulta:

- **Morbidade hospitalar:** diagnósticos, internações, óbitos e permanência.
- **Produção hospitalar:** procedimentos, AIH e valores registrados.

Uma mesma pessoa pode aparecer em mais de uma autorização. Por isso, escreva “internações” ou “AIH”, e não “pacientes”, ao apresentar o resultado.

## SIA/SUS: produção ambulatorial

O SIA reúne consultas, exames, tratamentos e outros procedimentos ambulatoriais apresentados ou aprovados pelo SUS.

Se o resultado mostrar 1.000 procedimentos, isso não significa automaticamente 1.000 pessoas. Uma pessoa pode realizar vários procedimentos na mesma competência.

## SINAN: agravos de notificação

O SINAN reúne notificações de doenças e agravos, como dengue. Cada agravo tem formulário, definição de caso, período e campos próprios.

Evite somar ou comparar agravos diferentes sem antes consultar suas notas técnicas. Um caso notificado também pode ser descartado após investigação, dependendo do filtro escolhido.

## CNES: estabelecimentos e capacidade

O CNES descreve hospitais, unidades básicas, profissionais, leitos, equipamentos, serviços e equipes cadastrados.

Cada competência funciona como uma fotografia daquele mês. Somar janeiro e fevereiro contaria novamente muitos dos mesmos estabelecimentos. Por isso, quando vários meses são escolhidos, o mapa usa a competência mais recente.

O mapa de pontos usa dados georreferenciados do `geobr`, derivados do CNES. Alguns registros podem ficar fora do mapa por coordenada ausente ou inválida.

## SINASC: nascidos vivos

O SINASC permite analisar nascidos vivos por residência da mãe ou ocorrência,
baixo peso, prematuridade, tipo de parto e anomalia registrada. Proporções usam como
denominador apenas os registros com resposta conhecida e apresentam separadamente a
quantidade ignorada. A taxa de nascidos vivos usa multiplicador de mil habitantes.

## População para calcular taxas

O denominador vem de fontes oficiais:

| Ano | Fonte usada |
|---|---|
| 1996-1999 | Sem denominador municipal SIDRA configurado; a taxa fica ausente |
| 2000 e 2010 | Censos Demográficos, IBGE/SIDRA |
| 2001-2006, 2008-2009 e 2011-2021 | Estimativas municipais, IBGE/SIDRA |
| 2022 | Censo Demográfico 2022, IBGE/SIDRA |
| 2007 e 2023 | Sem estimativa municipal anual compatível; a taxa fica ausente |
| 2024 em diante | Estimativas municipais, IBGE/SIDRA |

## Limites dos mapas

Estados, municípios e regiões de saúde vêm do `geobr`, com base em referências do IBGE e do Ministério da Saúde. O painel escolhe o limite disponível mais próximo do período analisado e informa o ano utilizado.

Limites podem mudar. Um município criado ou alterado depois do período analisado pode exigir cautela em comparações históricas.

Para regiões de saúde, a mesma referência territorial é usada no numerador, no denominador e no mapa durante toda a análise.

## Quando uma fonte fica indisponível

DATASUS, SIDRA e os arquivos espaciais são serviços externos. Eles podem ficar lentos, alterar arquivos ou interromper o acesso por algum tempo.

O painel nunca transforma uma falha de rede em zero. Zeros só são preenchidos depois que todos os arquivos esperados por UF e período foram identificados e lidos. O cache DBC distingue a URL de cada publicação, registra checksum SHA-256 e tenta renovar os arquivos após 24 horas, evitando confundir versões preliminares e revisadas com o mesmo nome. Se já existir um resultado agregado antigo e completo, ele pode ser usado como contingência quando a atualização falhar; nesse caso, a data do cache é informada.
