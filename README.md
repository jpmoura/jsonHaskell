# jsonHaskell
Algoritmo desenvolvido como parte dos créditos da disciplina CEA419 do curso de bacharelado de Sistemas de Informação da Universidade
Federal de Ouro Preto com foco na exploração de dados remotos no padrão JSON usando linguagem Haskell.

## Introdução

Aplicações para computadores pessoais ou para dispositivos móveis necessitam
efetuar trocas de informações através da internet. Para sanar esta necessidade, deve-se
utilizar algum protocolo de comunicação distribuída. O protocolo REST tem sido utilizado
para implementação de serviços na internet, uma vez que é bastante simples de ser
utilizado em comparação com protocolos como CORBA, RPC e SOAP.

Visando a diminuir o tráfego de dados nestas trocas de informações, desenvolveu-se
o formato de arquivo JSON, mais enxuto que o formato de arquivo XML, utilizado no
protocolo SOAP, por exemplo. O acesso aos serviços disponibilizados segundo o protocolo REST se dá através dos
comandos GET, POST, PUT, PATCH e DELETE do protocolo HTTP; arquivos JSON são
enviados e recebidos para transportar os dados desejados.

## O Trabalho

Neste trabalho, utilizamos a linguagem Haskell para explorar os dados de um
supermercado de forma remota. São realizadas consultas e inserções de dados em um
serviço remoto, que implementa o protocolo REST, através de arquivos JSON.

Foi explorado um sistema de Supermercado, no qual existem diversos produtos
cadastrados sob setores diferentes, além de serviços para cadastro. Através de URI
específicas para cada setor e para os serviços de inserção de dados, você deve executar as
tarefas descritas mais adiante.

## Ambiente de Desenvolvimento
### Arquivo JSON

O arquivo JSON a ser utilizado deve ser baixado através desse [link](http://beta.jsongenerator.
com/api/json/get/EyQVvjbp) e salvo localmente com o nome supermercado.json.
A cada interação de teste envolvendo submissão, sugere-se reiniciar o servidor e
utilizar o arquivo original para uma nova execução.

### Servidor JSON local

O servidor JSON utilizado encontra-se [aqui](https://github.com/typicode/jsonserver).
Este servidor tem como pré-requisito a instalação do NODE.JS, que pode ser obtida [aqui](https://nodejs.org/en/).
Após a instalação dos programas, a execução do servidor é realizada pelo comando:

``` bash
json-server --watch supermercado.json
```

### Bibliotecas do Haskell

Para explorar os dados no formato JSON, foi utilizada a biblioteca [aeson](https://hackage.haskell.org/package/aeson) e para a geração de gráficos 2D foi usada a biblioteca [haskell-chart](https://
github.com/timbod7/haskell-chart/wiki). O projeto utilizou o GHC 7.10.2 e o sistema [Cabal](https://
www.haskell.org/cabal/) para geração de um pacote [instalável](https://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program).

## Tarefas Desenvolvidas

1. Gerar um gráfico de barras com o número de fornecedores ativos e inativos (Y:
número de fornecedores; X: em atividade ou não).
2. Gerar um gráfico mostrando quantos distribuidores existem para cada um dos 10
produtos (Y: nome do produto; X: quantidade de distribuidores).
3. Gerar um gráfico mostrando quantos produtos cada distribuidor fornece (Y:
quantidade de produtos; X: distribuidores), para os 5 maiores distribuidores (aqueles
com mais produtos listados).
4. Gerar lista de compras para reabastecer o estoque informando, para cada
produto, qual distribuidor deve ser utilizado e o preço unitário do produto neste
distribuidor. O distribuidor escolhido deverá ser o que fornece o produto pelo menor
preço e encontra-se ativo no sistema. Os itens desta lista de compra devem ser
submetidos na rota listaDeCompras do arquivo JSON, sendo que cada item deve ter os
seguintes campos: id (gerado automaticamente pelo servidor no momento da
submissão), produto (contendo o nome do produto), distribuidor (contendo o nome do
distribuidor), preço, telefone. Esta lista de compras também deve ser impressa no
terminal a partir de uma consulta JSON aos dados que foram enviados ao servidor.
