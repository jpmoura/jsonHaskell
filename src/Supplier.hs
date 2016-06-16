{- Supplier.hs
Trabalho de Haskell - Primeira Entrega

Conteúdo referente as atividades 1 e 2 do trabalho prático de Haskell

Nome: João Pedro Santos de Moura
Nome: Marina Rocha Maia

Data: 20/01/2016
-}

{-# LANGUAGE OverloadedStrings #-}

module Supplier where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Text
import Network.HTTP.Conduit (simpleHttp)

data Supplier =
    Supplier {
    cep                 :: Text
    , telefone          :: Text
    , endereco          :: Text
    , nome              :: Text
    , ativo             :: Bool
    , supplierId        :: Int
    } deriving (Show)

instance FromJSON Supplier where
    parseJSON (Object v) =
        Supplier <$>
        (v.: "cep")    <*>
        (v.: "telefone") <*>
        (v.: "endereco") <*>
        (v.: "nome") <*>
        (v.: "ativo") <*>
        (v.: "id")
    parseJSON _ = mzero

-- Get the json data as a ByteString object
getSupplierFromJSON ::  IO BS.ByteString
getSupplierFromJSON = simpleHttp "http://localhost:3000/distribuidores"

-- Decode the ByteString object as Supplier object
decodeSuppliers = liftM decode getSupplierFromJSON -- Lint suggestion
--getSuppliers = getSupplierFromJSON  >>= return . decode  :: IO (Maybe [Supplier])

-- Rid the Maybe and return just a list of supplier
ridMaybefromSupplier :: Maybe [Supplier]->[Supplier]
ridMaybefromSupplier list = case list of
  Just list -> list
  otherwise -> []

-- Get a list of Supplier objects
getSuppliers = do
  rawSuppliers <- decodeSuppliers -- :t Maybe [Supplier]
  let suppliers = ridMaybefromSupplier rawSuppliers -- :t [Supplier]
  return suppliers

-- Get the amount of active or inactive suppliers
countStateSuppliers :: [Supplier]->Bool->Int->Int
countStateSuppliers [] _ _ = 0
countStateSuppliers (x:xs) state amount
  | ativo x == state = 1 + countStateSuppliers xs state amount
  | otherwise = countStateSuppliers xs state amount

-- Format the output so it can be printed in the chart
formatActiveAndInactive :: Int->Int->[(String,[Int])]
formatActiveAndInactive x1 x2 = [("Ativos",[x1]), ("Inativos",[x2])]
