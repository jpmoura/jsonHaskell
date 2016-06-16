{- Fruit.hs
Trabalho de Haskell - Primeira Entrega

Conteúdo referente as atividades 1 e 2 do trabalho prático de Haskell

Nome: João Pedro Santos de Moura
Nome: Marina Rocha Maia

Data: 20/01/2016
-}

{-# LANGUAGE OverloadedStrings #-}

module Fruit where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Data.Ord
import Data.Text
import Network.HTTP.Conduit (simpleHttp)


data Fruit =
    Fruit {
      preco           :: Text
      , distribuidor  :: Text
      , produto       :: Text
      , frutaId       :: Int
    } deriving (Show)

instance FromJSON Fruit where
    parseJSON (Object v) =
        Fruit <$>
        (v.: "preco")    <*>
        (v.: "distribuidor") <*>
        (v.: "produto") <*>
        (v.: "id")
    parseJSON _ = mzero

getFruitsFromJSON ::  IO BS.ByteString
getFruitsFromJSON = simpleHttp "http://localhost:3000/frutas"

getSuppListFromJSON ::  IO BS.ByteString
getSuppListFromJSON = simpleHttp "http://localhost:3000/_distrib"

-- Decode the ByteString object as Maybe [Fruit] object
decodeFruits = liftM decode getFruitsFromJSON -- Lint suggestion
--decodeFruits = getFruitsFromJSON  >>= return . decode  :: IO (Maybe [Fruit])

decodeSuppList = getSuppListFromJSON  >>= return . decode  :: IO (Maybe [Text])

-- Rid the Maybe and return just a list of Fruit
ridMaybeFromFruit :: Maybe [Fruit]->[Fruit]
ridMaybeFromFruit list = case list of
  Just list -> list
  otherwise -> []

ridMaybeFromSuppList :: Maybe [Text]->[Text]
ridMaybeFromSuppList list = case list of
  Just list -> list
  otherwise -> []

-- Get a list of Fruit objects
getFruits = do
  rawFruits <- decodeFruits -- :t Maybe [Fruit]
  let fruits = ridMaybeFromFruit rawFruits -- :t [Fruit]
  return fruits

-- Get suppliers list
getSuppList = do
  rawSuppList <- decodeSuppList
  let suppList = ridMaybeFromSuppList rawSuppList
  return suppList

---------------------- EM TESTE ------------------------------------------

countField :: (Fruit->Text)->[Fruit]->Text->Int->Int
countField _ [] _ _ = 0
countField field list@(x:xs) valor acc
  | (field x) == valor = 1 + countField field xs valor acc
  | otherwise = countField field xs valor acc

--------------------------- EM TESTE ----------------------------------



-- Get the suppliers amount of a fruit
countFruitsSuppliers :: [Fruit]->Text->Int->Int
countFruitsSuppliers [] _ _ = 0
countFruitsSuppliers (x:xs) fruit amount
  | produto x == fruit = 1 + countFruitsSuppliers xs fruit amount
  | otherwise = countFruitsSuppliers xs fruit amount

-- Get the amount of products supplied from a supplier
countSuppliersProducts :: [Fruit]->Text->Int->Int
countSuppliersProducts [] _ _ = 0
countSuppliersProducts (x:xs) supplier amount
  | distribuidor x == supplier = 1 + countSuppliersProducts xs supplier amount
  | otherwise = countSuppliersProducts xs supplier amount

-- Create a list with the number of products supplied from each Supplier
getSuppliersProductCount :: [Fruit]->[Text]->[Int]
getSuppliersProductCount _ [] = []
--getSuppliersProductCount fruitList (x:xs) = (countField (produto) fruitList x 0) : (getSuppliersProductCount fruitList xs)
getSuppliersProductCount fruitList (x:xs) = countSuppliersProducts fruitList x 0 : getSuppliersProductCount fruitList xs

-- Get the top 5 suppliers, the ones who supllies more products
getBestSuppliers fruitList = do
  suppList <- getSuppList -- Get a list of suppliers
  let suppProductCount = getSuppliersProductCount fruitList suppList -- get how many product each supplier supllies
  --print suppProductCount -- debug
  let result = Data.List.take 5 (sortBy (flip (comparing snd)) (Prelude.zip suppList suppProductCount)) -- take the top 5 from a ordered list
  return result

-- Format the output so it can be printed in the chart
formatProductCount :: [(Text,Int)]->[(String,[Int])]
formatProductCount [] = []
formatProductCount (x:xs) = (unpack (fst x), [snd x]) : formatProductCount xs

getAllProducts :: [Fruit]->[Text]
getAllProducts [] = []
getAllProducts (x:xs) = produto x : getAllProducts xs

getUniqueProducts :: [Fruit]->[Text]
getUniqueProducts fruitsList = nub (getAllProducts fruitsList)

-- Format the output so it can be printed in the chart
formatAmountOfSuppliers :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)->[(String,[Int])]
formatAmountOfSuppliers (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) =
  [
    ("Banana", [x1]),
    ("Figo", [x2]),
    ("Laranja", [x3]),
    ("Lima", [x4]),
    ("Limão", [x5]),
    ("Maçã", [x6]),
    ("Melancia", [x6]),
    ("Nectarina", [x8]),
    ("Nespera", [x9]),
    ("Pêra", [x10])
 ]
