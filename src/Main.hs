{-
Trabalho de Haskell - Primeira Entrega

Conteúdo referente as atividades 1 e 2 do trabalho prático de Haskell

Nome: João Pedro Santos de Moura
Nome: Marina Rocha Maia

Data: 20/01/2016
-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Time
import Fruit
import Graphics.Rendering.Chart.Easy as RCE
import Graphics.Rendering.Chart.Backend.Diagrams
import ShoppingList
import Supplier
import Network.Wreq

myTime :: String
myTime = "Tempo de execuçao: "

generateChart :: String->String->[(String,[Int])]->IO()
generateChart fileName chartTitle values = toFile def fileName $ do
  layout_title RCE..= chartTitle
  layout_title_style . font_size RCE..= 15
  layout_x_axis . laxis_generate RCE..= autoIndexAxis (Prelude.map fst values)
  plot $ fmap plotBars $ bars [""] $ addIndexes (Prelude.map snd values)

main :: IO ()
main = do
  putStrLn "Iniciando programa.\n"
  t0 <- Data.Time.getCurrentTime
  suppliersList <- getSuppliers
  fruitsList <- getFruits



  -- Parte 1
  putStrLn "---> Construçao do primeiro gráfico <---"
  t1 <- Data.Time.getCurrentTime

  putStrLn "1. Obtendo fornecedores ativos."
  let active = countStateSuppliers suppliersList True 0

  putStrLn "2. Obtendo fornecedores inativos."
  let inactive = countStateSuppliers suppliersList False 0

  putStrLn "3. Gerando gráfico"
  let values1 = formatActiveAndInactive active inactive
  generateChart "Grafico1.svg" "Fornecedores Ativos e Inativos" values1

  t2 <- Data.Time.getCurrentTime
  let diff1 = diffUTCTime t2 t1
  let timming1 = realToFrac diff1
  putStrLn (myTime ++ show timming1 ++ " segundos.")

  putStrLn "---> Construçao do primeiro gráfico concluída <---\n"

  -- Parte 2

  putStrLn "---> Construçao do segundo gráfico <---"

  putStrLn "1. Obtendo fornecedores para cada produto."
  t3 <- Data.Time.getCurrentTime
  let bananaAmount = countFruitsSuppliers fruitsList "banana" 0
  let figoAmount = countFruitsSuppliers fruitsList "figo" 0
  let laranjaAmount = countFruitsSuppliers fruitsList "laranja" 0
  let limaAmount = countFruitsSuppliers fruitsList "lima da pérsia" 0
  let limaoAmount = countFruitsSuppliers fruitsList "limão" 0
  let macaAmount = countFruitsSuppliers fruitsList "maçã" 0
  let melanciaAmount = countFruitsSuppliers fruitsList "melancia" 0
  let nectarinaAmount = countFruitsSuppliers fruitsList "nectarina" 0
  let nesperaAmount = countFruitsSuppliers fruitsList "nêspera" 0
  let peraAmount = countFruitsSuppliers fruitsList "pêra" 0

  putStrLn "2. Gerando gráfico."
  let values2 = formatAmountOfSuppliers(bananaAmount,figoAmount,laranjaAmount,limaAmount,limaoAmount,macaAmount,melanciaAmount,nectarinaAmount,nesperaAmount,peraAmount)
  generateChart "Grafico2.svg" "Quantidade de Fornecedores X Produto" values2

  t4 <- Data.Time.getCurrentTime
  let diff2 = diffUTCTime t4 t3
  let timming2 = realToFrac diff2
  putStrLn (myTime ++ show timming2 ++ " segundos.")
  putStrLn "---> Construcao do segundo grafico concluida <---\n"


  -- Parte 3
  putStrLn "---> Construçao do terceiro gráfico <---"
  putStrLn "1. Obtendo os 5 maiores distribuidores."
  t5 <- Data.Time.getCurrentTime
  result <- getBestSuppliers fruitsList
  let values3 = formatProductCount result

  putStrLn "2. Gerando gráfico."
  generateChart "Grafico3.svg" "Quantidade de Produtos X Fornecedor" values3

  t6 <- Data.Time.getCurrentTime
  let diff3 = diffUTCTime t6 t5
  let timming3 = realToFrac diff3
  putStrLn (myTime ++ show timming3 ++ " segundos.")
  putStrLn "---> Construçao do terceiro gráfico concluída <---\n"

  -- Parte 4
  putStrLn "---> Construçao da Lista de Compras <---"
  t7 <- Data.Time.getCurrentTime

  putStrLn "1. Obtendo lista de produtos."
  let allProducts = getUniqueProducts fruitsList

  putStrLn "2. Criando a lista de comrpas."
  let shoppingList = makeShoppingList allProducts fruitsList suppliersList

  putStrLn "3. Preparando lista para ser enviada ao banco de dados."
  let sendCommands = sendShoppingList shoppingList

  putStrLn "4.1 Enviando lista."
  sequence_ sendCommands
  putStrLn "4.2 Lista enviada."

  putStrLn "5. Obtendo lista de compras do servidor."
  shoppingListFromServer <- getShoppingList
  print shoppingListFromServer

  t8 <- Data.Time.getCurrentTime
  let diff4 = diffUTCTime t8 t7
  let timming4 = realToFrac diff4
  putStrLn (myTime ++ show timming4 ++ " segundos.")
  putStrLn "---> Criaçao da lista e envio para o servidor concluídos <---\n"


  tFinal <- Data.Time.getCurrentTime
  let diffTotal = diffUTCTime tFinal t0
  let timmingTotal = realToFrac diffTotal
  putStrLn ("Tempo total de execuçào: " ++ show timmingTotal ++ " segundos.")
  putStrLn "Programa encerrado."
