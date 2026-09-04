# Guia de primeiros passos

Este guia é para quem nunca usou dados do SUS, R ou um painel estatístico. Você não
precisa decorar siglas nem escrever código para fazer uma análise.

> **A ideia em uma linha:** escolha uma pergunta, defina o recorte, analise, confira os
> avisos e só então exporte o resultado.

## O caminho da primeira análise

```text
PERGUNTA → TEMA → LUGAR → PERÍODO → MEDIDA → ANALISAR → CONFERIR → EXPORTAR
```

Pense no painel como uma lupa. Primeiro você escolhe onde apontar a lupa; depois escolhe
o que quer contar ou comparar.

## Antes de começar

Confira três itens:

- o painel está aberto no navegador;
- o terminal que iniciou o painel continua aberto;
- há conexão com a internet para a primeira consulta.

Se o sistema ainda não estiver instalado, siga a seção
[Preparação do ambiente](../README.md#preparação-do-ambiente).

## Roteiro guiado de 10 minutos

Vamos consultar internações por município em uma UF. Esse é apenas um exercício para
conhecer a tela; ele não é uma conclusão epidemiológica.

1. Mantenha **Nível de configuração** em **Simples**.
2. Em **Tema**, escolha **Morbidade hospitalar (SIH)**.
3. Em **Conjunto de dados**, escolha **Morbidade hospitalar geral**.
4. Em **Abrangência**, escolha uma UF, por exemplo **Acre**.
5. Em **Geografia do mapa**, escolha **Município**.
6. Clique em **Carregar opções da fonte**.
7. Aguarde a mensagem verde. Novos campos aparecerão na lateral.
8. Em **Medida**, escolha **Internações (AIH)**.
9. Em **Período(s)**, mantenha apenas um período mostrado pelo painel.
10. Em **Métrica exibida**, mantenha **Valor observado**.
11. Clique em **Analisar** e aguarde a mensagem **Consulta concluída**.

Ao terminar, você deverá ver quatro cartões, uma série temporal e um ranking. Abra a aba
**Mapas** para ver os mesmos dados por território e **Dados e exportação** para conferir
a tabela que alimenta o mapa.

### Por que existem dois botões?

| Botão | O que ele faz |
|---|---|
| **Carregar opções da fonte** | Monta as medidas, períodos e filtros permitidos para o tema e a geografia escolhidos |
| **Analisar** | Confirma os arquivos realmente publicados, baixa o necessário e calcula o resultado |

A lista de períodos representa períodos que podem existir segundo o calendário da fonte.
A existência de cada arquivo é confirmada ao clicar em **Analisar**. Se o arquivo ainda
não foi publicado, o painel avisa e não cria um zero falso.

Se você mudar **Tema**, **Conjunto de dados**, **Abrangência** ou **Geografia do mapa**,
clique novamente em **Carregar opções da fonte** antes de analisar.

## Comece pela pergunta, não pela sigla

| Quero saber... | Tema sugerido | Medida típica |
|---|---|---|
| Quantas internações ocorreram? | Morbidade hospitalar (SIH) | Internações (AIH) |
| Quais diagnósticos aparecem mais nas internações? | Morbidade hospitalar (SIH) | Internações, com filtro CID-10 |
| Quantos óbitos foram registrados? | Mortalidade (SIM) | Óbitos |
| Quantos procedimentos foram aprovados? | Produção ambulatorial (SIA) | Quantidade aprovada |
| Onde existem estabelecimentos ou leitos cadastrados? | Estrutura assistencial (CNES) | Estabelecimentos ou leitos |
| Quantas notificações de um agravo existem? | Agravos de notificação (SINAN) | Notificações |
| Qual a proporção de baixo peso ou prematuridade? | Nascidos vivos (SINASC) | Baixo peso ou prematuridade |

Não some resultados de sistemas diferentes como se fossem a mesma coisa. Uma AIH, uma
notificação e um procedimento aprovado são unidades diferentes.

## Residência ou ocorrência?

Essa escolha muda a pergunta respondida:

- **Residência** indica onde a pessoa morava.
- **Ocorrência** indica onde o óbito ou nascimento aconteceu.
- **Internação/processamento** indica onde o atendimento hospitalar foi registrado.
- **Estabelecimento** indica onde o serviço ou recurso estava cadastrado.

Exemplo: um morador do interior pode ser internado na capital. O mapa por residência e o
mapa por internação são corretos, mas contam histórias diferentes. Leia a frase de
**semântica territorial** mostrada junto ao resultado antes de comparar lugares.

## Total, taxa ou proporção?

### Valor observado: “quantos?”

Use para medir volume: internações, óbitos, procedimentos, leitos ou valores financeiros.
Um território populoso costuma ter totais maiores apenas por ter mais habitantes.

### Taxa bruta: “quanto em relação ao tamanho da população?”

Use para comparar territórios de tamanhos diferentes. De forma simplificada:

```text
taxa = ocorrências ÷ população × multiplicador
```

Taxa não é porcentagem e não prova risco individual. Ela também depende de existir uma
população oficial compatível com o ano escolhido.

### Taxa padronizada por idade: “como comparar estruturas etárias diferentes?”

É uma comparação ajustada por faixas etárias, disponível apenas para medidas elegíveis.
Nesta versão, os denominadores municipais por idade estão configurados para 2010. Em
outros anos, o painel informa a limitação e usa a taxa bruta quando possível.

### Proporção: “qual parte dos registros elegíveis?”

Medidas como baixo peso no SINASC usam apenas registros com resposta conhecida no
denominador. Confira **registros elegíveis** e **registros ignorados** antes de interpretar
o percentual.

## O que existe em cada aba

### Visão geral

- **Total registrado:** soma ou indicador do recorte escolhido.
- **Competências/Taxa geral:** quantidade de períodos ou taxa do conjunto.
- **Maior valor territorial:** território com maior valor na métrica exibida.
- **Territórios avaliados:** quantos possuem valor, zero confirmado ou ausência.
- **Insights auditáveis:** descrições produzidas por regras visíveis, sem afirmar causa.
- **Qualidade e completude:** contagem de valores ausentes e problemas de ligação.

Os rótulos abreviados em gráficos estreitos aparecem completos na tabela equivalente,
disponível em **Ver dados equivalentes aos gráficos**.

### Mapas

No mapa territorial:

- cor mais escura significa valor maior dentro da classificação escolhida;
- **Zero** significa arquivo confirmado sem ocorrência compatível;
- **Sem dado**, em cinza, significa valor, denominador ou ligação indisponível;
- clicar no território mostra total, taxa, intervalo, referência, período e geografia;
- o mapa-base pode sumir quando o OpenStreetMap está sem conexão, mas os polígonos e a
  tabela continuam sendo a parte analítica do resultado.

Não compare apenas a intensidade das cores entre dois mapas diferentes. Cada mapa pode
ter limites de classe próprios.

### Dados e exportação

A tabela territorial é a melhor forma de conferir o número exato visto no mapa. Use a
caixa no topo das colunas para pesquisar e a barra horizontal para ver todas as medidas.

## Quando usar o modo Avançado

Comece no modo **Simples**. Abra o **Avançado** somente quando precisar destas decisões:

| Controle | Quando usar |
|---|---|
| Referência automática | Para comparar municípios/regiões com a UF, ou UFs com o Brasil |
| Quantis | Melhor escolha geral quando existem valores muito diferentes |
| Intervalos iguais | Quando intervalos numéricos iguais têm significado para a pergunta |
| Logarítmica | Quando poucos valores extremos escondem a variação dos demais |
| Limites fixos | Quando você já possui limites técnicos definidos; não invente faixas de risco |
| Top 10/15/25 | Para controlar o tamanho do ranking |
| Limpar resultados agregados | Para forçar novo cálculo sem apagar os DBC já verificados |

## Qual arquivo devo exportar?

| Necessidade | Arquivo indicado |
|---|---|
| Abrir no LibreOffice ou Excel | CSV |
| Usar em QGIS ou outro programa de mapas | GeoJSON |
| Colocar uma figura em apresentação | PNG |
| Explorar o mapa fora do painel | Mapa HTML |
| Compartilhar análise, método e gráficos em um único arquivo | Relatório completo HTML |
| Reproduzir ou auditar exatamente a consulta | Manifesto JSON |

Para um trabalho ou relatório, guarde pelo menos o **Relatório completo HTML**, o CSV
territorial e o **Manifesto JSON**.

## Checklist antes de divulgar um número

Antes de copiar um resultado, confirme:

- [ ] qual sistema e medida foram usados;
- [ ] se o território é residência, ocorrência, internação ou estabelecimento;
- [ ] qual período foi selecionado;
- [ ] se o valor é total, taxa ou proporção;
- [ ] se há avisos metodológicos ou muitos dados ignorados;
- [ ] se zero e ausência foram interpretados de forma diferente;
- [ ] se o relatório e o manifesto foram guardados.

## Problemas comuns

| O que apareceu | O que fazer |
|---|---|
| “Os campos de fonte ou geografia mudaram” | Clique novamente em **Carregar opções da fonte** |
| Arquivo/período indisponível | Escolha um período anterior ou tente novamente mais tarde |
| Taxa indisponível | Use valor observado ou confira se existe denominador para o ano |
| Mapa cinza ou território ausente | Abra a tabela e o painel de qualidade; não trate como zero |
| Mapa-base não carregou | Confira a internet; os dados territoriais ainda podem estar disponíveis |
| Consulta nacional muito lenta | Teste primeiro uma UF e um único período |
| Pontos CNES não aparecem | Selecione uma UF, execute a análise e clique em **Carregar pontos** |
| Resultado parece antigo | Leia o aviso de cache; no modo Avançado, limpe os resultados agregados |

Se o painel inteiro não abrir, consulte as
[Perguntas frequentes](../README.md#perguntas-frequentes).

## Mini-glossário

| Palavra | Tradução prática |
|---|---|
| AIH | Registro administrativo de uma internação autorizada |
| Competência | Mês de referência do registro |
| CID-10 | Classificação usada para doenças e causas de morte |
| Denominador | População ou conjunto elegível usado na divisão |
| DBC | Arquivo comprimido distribuído pelo DATASUS |
| IC95% | Faixa que comunica a incerteza de uma taxa |
| Ranking | Lista ordenada dos maiores valores |
| Sem dado/NA | Informação não disponível; não significa zero |

## Para continuar aprendendo

- [Conheça as fontes de dados](fontes-de-dados.md)
- [Entenda os cálculos e limitações](metodologia.md)
- [Volte ao guia de instalação e perguntas frequentes](../README.md)
