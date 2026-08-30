# Projeto Data SUS

O Projeto Data SUS transforma dados públicos de saúde em tabelas, gráficos e mapas que podem ser explorados no navegador. Ele foi pensado para funcionar no seu próprio computador, sem enviar arquivos pessoais e sem exigir conhecimento prévio de programação ou estatística.

> **Resumo rápido:** instale o R, baixe este projeto, execute `Rscript prepare_environment.R` e abra o painel. O preparador cuida dos pacotes e das bibliotecas necessárias no Ubuntu ou Debian.

## Objetivo do projeto

O objetivo é facilitar perguntas como:

- Quantas internações foram registradas em cada município?
- Como os óbitos mudaram ao longo dos anos?
- Quais procedimentos ambulatoriais aparecem com maior frequência?
- Onde estão hospitais, unidades de urgência e unidades básicas de saúde?
- Qual é a taxa por 100 mil habitantes em cada território?

O painel reúne cinco fontes principais do SUS:

| Tema no painel | Sistema | O que pode ser analisado |
|---|---|---|
| Mortalidade | SIM | Óbitos e causas de morte |
| Morbidade hospitalar | SIH | Internações, diagnósticos e óbitos hospitalares |
| Produção hospitalar | SIH | AIH, procedimentos e valores registrados |
| Produção ambulatorial | SIA | Procedimentos e quantidades aprovadas |
| Agravos de notificação | SINAN | Casos notificados, como dengue |
| Estrutura assistencial | CNES | Estabelecimentos, leitos, profissionais e equipamentos |

Neste projeto, a palavra **demanda** significa utilização ou produção registrada no SUS. O sistema não mede filas, procura que não virou atendimento nem necessidade de saúde não atendida.

## Preparação do ambiente

Esta preparação automática foi feita para **Ubuntu e Debian**. Você só precisa instalar o R antes, pois um arquivo escrito em R não consegue ser executado sem o próprio R.

### Etapa 1: instalar o R

1. Abra o aplicativo **Terminal**. No Ubuntu, o atalho costuma ser `Ctrl + Alt + T`.
2. Digite o comando abaixo e pressione `Enter`:

```bash
sudo apt update && sudo apt install -y r-base r-base-dev
```

3. Se uma senha for solicitada, digite a senha do computador e pressione `Enter`. Nada aparece na tela enquanto a senha é digitada; isso é normal.
4. Confirme a instalação:

```bash
R --version
```

O projeto aceita R 4.1 ou mais recente. O ambiente reproduzível atual foi criado com R 4.6.1.

### Etapa 2: baixar o projeto

#### Opção mais simples: baixar um arquivo ZIP

1. Acesse [github.com/paulopimenta6/projeto_data_sus](https://github.com/paulopimenta6/projeto_data_sus).
2. Clique no botão verde **Code**.
3. Clique em **Download ZIP**.
4. Abra a pasta de downloads e extraia o arquivo ZIP.
5. Entre na pasta extraída chamada `projeto_data_sus`.
6. Clique com o botão direito em uma área vazia da pasta e escolha **Abrir no Terminal**.

#### Opção com Git

Se você já usa Git, execute:

```bash
git clone https://github.com/paulopimenta6/projeto_data_sus.git
cd projeto_data_sus
```

### Etapa 3: executar o preparador automático

No terminal aberto dentro da pasta do projeto, execute:

```bash
Rscript prepare_environment.R
```

O script realiza estas tarefas sozinho:

1. Confere se o sistema é Ubuntu ou Debian e se a versão do R é compatível.
2. Configura uma fonte segura para baixar pacotes e amplia o tempo permitido para downloads grandes.
3. Descobre e instala bibliotecas do sistema operacional que estiverem faltando.
4. Instala os preparadores `renv` e `pak`, se necessário.
5. Restaura as versões registradas em `renv.lock`.
6. Confere os pacotes, as versões e a criação da interface.

O Ubuntu pode pedir a senha de administrador uma vez. O processo pode levar de alguns minutos a mais de meia hora, principalmente na primeira instalação do DuckDB. Não feche o terminal enquanto a preparação estiver em andamento.

Quando aparecer a mensagem **“Tudo pronto!”**, o ambiente está preparado. O comando é seguro para ser executado novamente: itens que já estão corretos são reaproveitados.

Para apenas verificar o ambiente, sem instalar pacotes ou bibliotecas, use:

```bash
Rscript prepare_environment.R --check-only
```

O arquivo antigo `scripts/setup.R` continua funcionando, mas apenas encaminha a execução para o novo preparador.

### Etapa 4: abrir o sistema

Execute:

```bash
Rscript -e 'shiny::runApp(".", launch.browser = TRUE)'
```

O navegador deve abrir automaticamente. Se isso não acontecer, copie o endereço mostrado no terminal, normalmente parecido com `http://127.0.0.1:xxxx`, e abra-o no navegador.

Para encerrar o sistema, volte ao terminal e pressione `Ctrl + C`.

## Como usar o sistema

### Conhecendo a tela

O painel tem uma área de filtros e quatro abas:

- **Visão geral:** mostra indicadores, série temporal e ranking.
- **Mapas:** mostra o mapa territorial e os estabelecimentos do CNES.
- **Dados e exportação:** mostra a tabela detalhada e os botões de download.
- **Metodologia:** resume como os números devem ser interpretados.

O fluxo normal tem dois botões importantes:

1. **Carregar opções da fonte:** consulta o formulário atual do DATASUS e apresenta medidas, períodos e filtros válidos.
2. **Analisar:** executa a consulta escolhida e monta os resultados.

### Exemplo 1: internações por município no Acre

1. Em **Tema**, escolha **Morbidade hospitalar (SIH)**.
2. Em **Conjunto de dados**, escolha **Morbidade hospitalar geral**.
3. Em **Abrangência**, escolha **Acre**.
4. Em **Geografia do mapa**, escolha **Município**.
5. Clique em **Carregar opções da fonte** e aguarde a mensagem verde.
6. Em **Medida**, escolha **Internações**.
7. Em **Período(s)**, escolha uma competência disponível, como `Mai/2026`.
8. Em **Métrica exibida**, mantenha **Total**.
9. Clique em **Analisar**.

A visão geral mostrará o total registrado, o município com maior valor, a série e o ranking. Na aba **Mapas**, cores mais escuras representam valores maiores para a medida selecionada.

### Exemplo 2: comparar territórios usando taxas

Repita o exemplo anterior, mas escolha **Taxa por 100 mil** antes de clicar em **Analisar**.

Use taxas quando quiser comparar territórios com populações muito diferentes. Um município grande pode ter mais internações em números absolutos, mas um município menor pode ter uma taxa proporcionalmente maior.

As taxas do painel são **brutas**, isto é, não são ajustadas por idade, sexo ou outras diferenças entre as populações.

### Exemplo 3: localizar hospitais e unidades mistas

1. Execute uma análise válida com uma UF selecionada.
2. Abra a aba **Mapas**.
3. Abra **Estabelecimentos CNES**.
4. Escolha **Hospitais e unidades mistas**.
5. Clique em **Carregar pontos**.
6. Clique em um ponto ou agrupamento para explorar os estabelecimentos.

O número dentro de um círculo representa vários estabelecimentos próximos. Aproxime o mapa para separar os pontos.

### Exemplo 4: analisar mortalidade no Brasil

1. Em **Tema**, escolha **Mortalidade (SIM)**.
2. Em **Abrangência**, mantenha **Brasil**.
3. Em **Geografia do mapa**, escolha **Unidade da Federação**.
4. Clique em **Carregar opções da fonte**.
5. Escolha **Óbitos**, um ano disponível e, se desejar, uma causa CID-10.
6. Clique em **Analisar**.

No SIM agregado por estado, a abrangência deve permanecer em Brasil. Para detalhar municípios, selecione uma UF e mude a geografia para Município.

### Exportar resultados

Depois de uma análise, abra **Dados e exportação**. Estão disponíveis:

- CSV da tabela territorial, da série e do ranking;
- GeoJSON do mapa e dos estabelecimentos;
- PNG da série, do ranking e do mapa;
- HTML do mapa interativo;
- JSON do manifesto da consulta.

O manifesto registra filtros, períodos, versões, fonte, horário e avisos metodológicos. Ele é útil para lembrar exatamente como um resultado foi produzido.

## Outras particularidades

### Como interpretar os mapas

- Uma cor mais escura significa valor maior para a métrica escolhida.
- Cinza ou **NA** significa que não houve valor compatível, não que o valor seja necessariamente zero.
- Um mapa de totais destaca volume; um mapa de taxas facilita comparação proporcional.
- Diferença de cor não prova que um lugar tem maior risco. Acesso, registro, encaminhamento e organização dos serviços também influenciam os números.
- O ano dos limites territoriais aparece no painel porque municípios e regiões de saúde podem mudar.

### Como interpretar os sistemas

- O SIH registra autorizações e internações, não pessoas únicas.
- O SIA registra produção ambulatorial aprovada, não pessoas únicas.
- O CNES mostra uma fotografia cadastral de cada competência; meses não são somados no mapa.
- O SINAN possui regras diferentes para cada agravo.
- Competências recentes podem estar incompletas ou ser revisadas.
- A disponibilidade de taxa depende de uma contagem válida e de população oficial compatível.

### População usada nas taxas

| Período | Denominador |
|---|---|
| Até 2021 | Estimativa municipal publicada no DATASUS |
| 2022 | Censo Demográfico 2022, SIDRA tabela 4709 |
| 2023 | Taxa omitida quando não existe denominador municipal oficial compatível |
| 2024 em diante | Estimativa municipal, SIDRA tabela 6579 |

Quando vários meses são somados, o painel usa pessoas-ano no denominador geral. Intervalos de 95% usam o método exato de Poisson quando o numerador é uma contagem inteira.

### Fontes e conexão com a internet

Os dados vêm de DATASUS/TABNET, IBGE/SIDRA e `geobr`. O sistema precisa de internet para uma consulta nova. Esses serviços podem ficar lentos ou temporariamente indisponíveis; o painel mostra uma mensagem em vez de transformar a falha em zero.

### Cache local

Resultados já consultados ficam em `data/cache/` para acelerar acessos posteriores. O cache contém respostas públicas e não é enviado ao GitHub.

Para apagar o cache, feche o sistema e remova o conteúdo da pasta `data/cache/`. A consulta seguinte será baixada novamente.

### Leituras complementares

- [Entenda a metodologia](docs/metodologia.md)
- [Conheça as fontes de dados](docs/fontes-de-dados.md)
- [Veja como as versões são controladas](docs/pacotes-e-versoes.md)

### Verificação para pessoas desenvolvedoras

```bash
Rscript scripts/check.R
RUN_LIVE_DATASUS_TESTS=true Rscript tests/testthat.R
```

O primeiro comando executa sintaxe, lint, testes e construção da interface sem depender do DATASUS. O segundo acrescenta uma verificação opcional contra o serviço ao vivo.

## Licença

O código usa a licença MIT. Os dados continuam sujeitos às condições, definições e notas técnicas das fontes oficiais.

## Perguntas frequentes

### 1. Preciso saber programar ou usar estatística?

Não. A preparação usa um comando e a análise é feita por menus e botões. As explicações deste guia ajudam a escolher entre total e taxa.

### 2. O que é R?

R é o programa que executa o painel e os cálculos. Você precisa instalá-lo uma vez antes de executar `prepare_environment.R`.

### 3. Por que o sistema pede a senha do computador?

Algumas bibliotecas de mapas e bancos de dados precisam ser instaladas pelo Ubuntu. O comando `sudo` solicita permissão de administrador. Nenhum caractere aparece enquanto você digita a senha; isso é uma proteção normal do terminal.

### 4. Apareceu “Rscript: comando não encontrado”. O que faço?

O R ainda não está instalado ou o terminal foi aberto antes da instalação terminar. Execute a Etapa 1, feche o terminal, abra outro e teste `R --version`.

### 5. Posso executar o preparador mais de uma vez?

Sim. Ele verifica o que já está correto e instala somente o que estiver ausente ou fora da versão registrada.

### 6. A instalação parece parada durante o DuckDB. É normal?

Sim. Em alguns computadores o DuckDB precisa ser compilado e pode ficar vários minutos mostrando mensagens parecidas. Aguarde enquanto houver atividade no terminal.

### 7. O DATASUS demorou ou retornou erro. O sistema está quebrado?

Provavelmente não. O TABNET pode ficar lento ou indisponível. Aguarde alguns minutos e tente novamente. Consultas anteriores podem continuar disponíveis no cache.

### 8. Por que o painel começa na penúltima competência mensal?

A competência mais recente publicada pode ainda não conter registros. O painel começa na penúltima para aumentar a chance de uma primeira consulta completa, mas todas continuam disponíveis.

### 9. Por que não aparece uma taxa?

A medida pode não ser uma contagem, a população pode estar indisponível ou o período pode incluir 2023 sem denominador municipal compatível. Nesse caso, o painel mantém os totais e mostra um aviso.

### 10. Uma área cinza no mapa significa zero?

Não necessariamente. Cinza indica ausência de valor compatível. Consulte a tabela e os avisos da análise antes de interpretar.

### 11. Por que preciso selecionar uma UF para os pontos do CNES?

O arquivo nacional possui centenas de milhares de estabelecimentos. O recorte por UF evita travar o navegador e torna o mapa mais rápido.

### 12. Como paro o sistema?

Volte ao terminal onde ele está executando e pressione `Ctrl + C`.

### 13. O navegador não abriu. Como acesso o painel?

Procure no terminal um endereço iniciado por `http://127.0.0.1:`. Abra esse endereço manualmente em Firefox, Chrome ou outro navegador atual.

### 14. Posso usar o painel sem internet?

Somente para itens que já estejam no cache. Novas consultas e novas geometrias precisam de internet.

### 15. Como verifico se a instalação continua correta?

Execute:

```bash
Rscript prepare_environment.R --check-only
```

Se o comando terminar com **“Tudo pronto!”**, os pacotes e a interface estão funcionando.

### 16. Posso usar o preparador no Windows ou macOS?

Não nesta versão. O preparador automático reconhece apenas Ubuntu e Debian. O código do painel é escrito em R, mas a instalação de bibliotecas do sistema precisaria de instruções específicas para Windows ou macOS.
