{- ShoppingList.hs
Trabalho de Haskell - Primeira Entrega

Conteúdo referente as atividades 3 e 4 do trabalho prático de Haskell

Nome: João Pedro Santos de Moura
Nome: Marina Rocha Maia

Data: 20/01/2016
-}

{-# LANGUAGE OverloadedStrings #-}

module ShoppingList where

import Control.Applicative ((<$>),(<*>))
import Fruit
import Control.Lens ((&),(.~))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Data.Ord
import Supplier
import Data.Text
import Data.Text.Read
import Network.HTTP.Conduit (simpleHttp)
import Network.Wreq

data ShoppingList =
    ShoppingList {
      slId                 :: Int
      , listaProduto       :: Text
      , listaDistribuidor  :: Text
      , listaPreco         :: Text
      , listaTelefone      :: Text
    } deriving (Show)


instance FromJSON ShoppingList where
    parseJSON (Object v) =
        ShoppingList <$>
        (v.: "id") <*>
        (v.: "produto")    <*>
        (v.: "distribuidor") <*>
        (v.: "preco") <*>
        (v.: "telefone")
    parseJSON _ = mzero

instance ToJSON ShoppingList where
 toJSON (ShoppingList slId listaProduto listaDistribuidor listaPreco listaTelefone) =
    object [ "produto"  .= listaProduto
           , "distribuidor"   .= listaDistribuidor
           , "preco"        .= listaPreco
           , "telefone" .= listaTelefone
             ]

getShoppingListFromJSON :: IO BS.ByteString
getShoppingListFromJSON = simpleHttp "http://localhost:3000/listaDeCompras"

--decodeShopList = liftM decode getShoppingListFromJSON -- Lint suggestion

decodeShoppingList = getShoppingListFromJSON  >>= return . decode  :: IO (Maybe [ShoppingList])

ridMaybeFromShoppingList :: Maybe [ShoppingList]->[ShoppingList]
ridMaybeFromShoppingList list = case list of
  Just list -> list
  otherwise -> []

-- Get a list of ShoppingList objects
getShoppingList = do
  rawShoppingList <- decodeShoppingList -- :t Maybe [ShoppingList]
  let shoppingList = ridMaybeFromShoppingList rawShoppingList -- :t [ShoppingList]
  return shoppingList

sendJSON2Server :: ShoppingList->IO (Response BS.ByteString)
sendJSON2Server x = postWith opts "http://localhost:3000/listaDeCompras" (encode x) where
  opts = defaults & header "content-type" .~ ["application/json"]

-- Get all suppliers names of a fruit
getAllSuppliersOf :: Text->[Fruit]->[Text]
getAllSuppliersOf _ [] = []
getAllSuppliersOf fruit (x:xs)
  | fruit == produto x = distribuidor x : getAllSuppliersOf fruit xs
  | otherwise = getAllSuppliersOf fruit xs

-- Checks if a supplier is active
isActive :: Text->[Supplier]->Bool
isActive _ [] = False
isActive supplier list@(x:xs)
  | supplier == nome x && ativo x = True
  | otherwise = isActive supplier xs

-- Delete all inactive suppliers from list
keepActiveOnly :: [Text]->[Supplier]->[Text]
keepActiveOnly [] _ = []
keepActiveOnly (x:xs) supplierList
  | isActive x supplierList = x : keepActiveOnly xs supplierList
  | otherwise = keepActiveOnly xs supplierList

-- Search for a supplier by its name
getSupplierByName :: Text->[Supplier]->Supplier
getSupplierByName name (x:xs)
  | name == nome x = x
  | otherwise = getSupplierByName name xs

-- Parse a Double froma Text in format '$XX.XX'
myParseFloat :: Text->Double
myParseFloat x = read n :: Double where
                                    n = unpack (Data.Text.tail x) -- delete '$' then cast to String

-- Get the price of a fruit by its supplier
getPriceOf :: Text->Text->[Fruit]->Double
getPriceOf _ _ [] = -1
getPriceOf fruit supplier (x:xs)
  | fruit == produto x && distribuidor x == supplier =  myParseFloat (preco x)
  | otherwise = getPriceOf fruit supplier xs

-- Get all prices of a product from suppliers list
getAllPrices :: Text->[Fruit]->[Text]->[(Text, Double)]
getAllPrices _ [] _ = []
getAllPrices _ _ [] = []
getAllPrices fruit products suppliers@(x:xs) = (supplier, getPriceOf fruit supplier products) : getAllPrices fruit products xs
                                                where
                                                  supplier = Prelude.head suppliers

-- Get the cheapest supplier of a product
getCheapest :: Text->[Fruit]->[Text]->(Text, Double)
getCheapest fruit products suppliers = minimumBy (comparing snd) (getAllPrices fruit products suppliers)

-- Creates a ShoppingList item that will be sent to JSON server
makeItem :: Text->(Text,Double)->[Supplier]->ShoppingList
makeItem fruit (supplier,price) supplierList = ShoppingList 0 fruit supplier (pack ("$" ++ (show price))) phone
                                  where
                                    phone = telefone (getSupplierByName supplier supplierList)

-- Create the shopping list
makeShoppingList :: [Text]->[Fruit]->[Supplier]->[ShoppingList]
makeShoppingList [] _ _ = []
makeShoppingList products@(x:xs) allProducts suppliers = makeItem x cheapest suppliers : makeShoppingList xs allProducts suppliers
                                      where
                                        allSuppliers = getAllSuppliersOf x allProducts
                                        activeOnly = keepActiveOnly allSuppliers suppliers
                                        cheapest = getCheapest x allProducts activeOnly

-- Create list of Send
sendShoppingList :: [ShoppingList]->[IO(Response BS.ByteString)]
sendShoppingList [] = []
sendShoppingList (x:xs) = sendJSON2Server x : sendShoppingList xs
