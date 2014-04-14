module NASDAQOMX.Nordic.OrderBook
    ( OrderBook(..)
    , empty
    , insert
    ) where

import qualified Data.Map as Map

type Quantity = Int

data Side = Buy | Sell deriving (Show)

data Entry = Entry
    { price    :: Double
    , quantity :: Quantity
    , side     :: Side
    } deriving (Show)

type PriceLevel = [Entry]

data OrderBook = OrderBook
    { bidLevels :: Map.Map Double PriceLevel
    , askLevels :: Map.Map Double PriceLevel
    } deriving (Show)

empty :: OrderBook
empty = OrderBook
    { bidLevels = Map.empty
    , askLevels = Map.empty
    }

insert :: OrderBook -> Entry -> OrderBook
insert ob entry = case (side entry) of
    Buy  -> undefined
    Sell -> undefined

