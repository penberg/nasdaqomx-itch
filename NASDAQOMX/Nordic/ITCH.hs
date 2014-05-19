-- |
-- Module      : NASDAQOMX.Nordic.ITCH
-- Copyright   : (c) Pekka Enberg, 2014
-- License     : BSD-style
--
-- A Haskell implementation of NASDAQ OMX Nordic ITCH protocol as specified in:
--
-- Nordic Equity TotalView-ITCH 
-- Version 1.90.1 
-- February 10, 2014 
--

module NASDAQOMX.Nordic.ITCH
    ( Message(..)
    , parseSoupFILE
    , parseMessage
    ) where

import qualified Data.Binary.Get.Internal as BSX (readN)
import qualified Data.ByteString.Internal as BS (w2c)
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Data.Char (isSpace)
import Data.Binary.Get
import Data.Word

type Numeric      = Maybe Int
type Alphanumeric = B.ByteString
type Alphabetic   = B.ByteString
type Price        = B.ByteString

data Message
    = Seconds
        { second                    :: Numeric }
    | Milliseconds
        { millisecond               :: Numeric }
    | SystemEvent
        { eventCode                 :: Alphanumeric }
    | MarketSegmentState
        { marketSegmentID           :: Numeric
        , eventCode                 :: Alphanumeric }
    | OrderBookDirectory
        { orderBook                 :: Numeric
        , symbol                    :: Alphanumeric
        , isin                      :: Alphanumeric
        , financialProduct          :: Numeric
        , tradingCurrency           :: Alphabetic
        , mic                       :: Alphabetic
        , marketSegmentID           :: Numeric
        , noteCodes                 :: Numeric
        , roundLotSize              :: Numeric }
    | OrderBookTradingAction
        { orderBook                 :: Numeric
        , tradingState              :: Alphabetic
        , reserved                  :: Alphanumeric
        , reason                    :: Alphanumeric }
    | AddOrder
        { orderReferenceNumber      :: Numeric
        , buySellIndicator          :: Alphabetic
        , quantity                  :: Numeric
        , orderBook                 :: Numeric
        , price                     :: Price }
    | AddOrderMPID
        { orderReferenceNumber      :: Numeric
        , buySellIndicator          :: Alphabetic
        , quantity                  :: Numeric
        , orderBook                 :: Numeric
        , price                     :: Price
        , attribution               :: Alphabetic }
    | OrderExecuted
        { orderReferenceNumber      :: Numeric
        , executedQuantity          :: Numeric
        , matchNumber               :: Numeric
        , ownerParticipantID        :: Alphabetic
        , counterpartyParticipantID :: Alphabetic }
    | OrderExecutedWithPrice
        { orderReferenceNumber      :: Numeric
        , executedQuantity          :: Numeric
        , matchNumber               :: Numeric
        , printable                 :: Alphabetic
        , tradePrice                :: Price
        , ownerParticipantID        :: Alphabetic
        , counterpartyParticipantID :: Alphabetic }
    | OrderCancel
        { orderReferenceNumber      :: Numeric
        , canceledQuantity          :: Numeric }
    | OrderDelete
        { orderReferenceNumber      :: Numeric }
    | Trade
        { orderReferenceNumber      :: Numeric
        , tradeType                 :: Alphabetic
        , quantity                  :: Numeric
        , orderBook                 :: Numeric
        , matchNumber               :: Numeric
        , tradePrice                :: Price
        , buyerParticipantID        :: Alphabetic
        , sellerParticipantID       :: Alphabetic }
    | CrossTrade
        { quantity                  :: Numeric
        , orderBook                 :: Numeric
        , crossPrice                :: Price
        , matchNumber               :: Numeric
        , crossType                 :: Alphabetic
        , numberOfTrades            :: Numeric }
    | BrokenTrade
        { matchNumber               :: Numeric }
    | NOII
        { pairedQuantity            :: Numeric
        , imbalanceQuantity         :: Numeric
        , imbalanceDirection        :: Alphabetic
        , orderBook                 :: Numeric
        , equilibriumPrice          :: Price
        , crossType                 :: Alphabetic
        , bestBidPrice              :: Price
        , bestBidQuantity           :: Numeric
        , bestAskPrice              :: Price
        , bestAskQuantity           :: Numeric }
    | Unknown
    | EOF
    deriving (Show, Ord, Eq)

parseSoupFILE :: Get Message
parseSoupFILE = do
    msgType <- getWord8
    case msgType of
        0x0d -> do
                 _ <- getWord8 -- LF
                 return EOF
        _ -> do
                 msg <- parseMessage msgType
                 _ <- getWord8 -- CR
                 _ <- getWord8 -- LF
                 return msg

getNumeric :: Int -> Get Numeric
getNumeric n = BSX.readN n $ toInt
  where
    toInt :: B.ByteString -> Numeric
    toInt bs = case B.readInt (B.dropWhile isSpace bs) of
        Just i -> Just $ fst i
        _      -> Nothing

parseMessage :: Word8 -> Get Message
parseMessage msgType =
    case BS.w2c msgType of
        'T' -> Seconds
                   <$> getNumeric    5  -- Second
        'M' -> Milliseconds
                   <$> getNumeric    3  -- Millisecond
        'S' -> SystemEvent
                   <$> getByteString 1  -- Event Code
        'O' -> MarketSegmentState
                   <$> getNumeric    3  -- Market Segment ID
                   <*> getByteString 1  -- Event Code
        'R' -> OrderBookDirectory
                   <$> getNumeric    6  -- Order Book
                   <*> getByteString 16 -- Symbol
                   <*> getByteString 12 -- ISIN
                   <*> getNumeric    3  -- Financial Product
                   <*> getByteString 3  -- Trading Currency
                   <*> getByteString 4  -- MIC
                   <*> getNumeric    3  -- Market Segment ID
                   <*> getNumeric    8  -- Note Codes
                   <*> getNumeric    9  -- Round Lot Size
        'H' -> OrderBookTradingAction
                   <$> getNumeric    6  -- Order Book
                   <*> getByteString 1  -- Trading State
                   <*> getByteString 1  -- Reserved
                   <*> getByteString 4  -- Reason
        'A' -> AddOrder
                   <$> getNumeric    9  -- Order Reference Number
                   <*> getByteString 1  -- Buy/Sell Indicator
                   <*> getNumeric    9  -- Quantity
                   <*> getNumeric    6  -- Order Book
                   <*> getByteString 10 -- Price
        'F' -> AddOrderMPID
                   <$> getNumeric    9  -- Order Reference Number
                   <*> getByteString 1  -- Buy/Sell Indicator
                   <*> getNumeric    9  -- Quantity
                   <*> getNumeric    6  -- Order Book
                   <*> getByteString 10 -- Price
                   <*> getByteString 4  -- Attribution
        'E' ->  OrderExecuted
                   <$> getNumeric    9  -- Order Reference Number
                   <*> getNumeric    9  -- Executed Quantity
                   <*> getNumeric    9  -- Match Number
                   <*> getByteString 4  -- Participant ID, owner
                   <*> getByteString 4  -- Participant ID, counterparty
        'C' -> OrderExecutedWithPrice
                   <$> getNumeric    9  -- Order Reference Number
                   <*> getNumeric    9  -- Executed Quantity
                   <*> getNumeric    9  -- Match Number
                   <*> getByteString 1  -- Printable
                   <*> getByteString 10 -- Trade Price
                   <*> getByteString 4  -- Participant ID, owner
                   <*> getByteString 4  -- Participant ID, counterparty
        'X' -> OrderCancel
                   <$> getNumeric    9  -- Order Reference Number
                   <*> getNumeric    9  -- Canceled Quantity
        'D' -> OrderDelete
                   <$> getNumeric    9  -- Order Reference Number
        'P' -> Trade
                   <$> getNumeric    9  -- Order Reference Number
                   <*> getByteString 1  -- Trade Type
                   <*> getNumeric    9  -- Quantity
                   <*> getNumeric    6  -- Order Book
                   <*> getNumeric    9  -- Match Number
                   <*> getByteString 10 -- Trade Price
                   <*> getByteString 4  -- Participant ID, buyer
                   <*> getByteString 4  -- Participant ID, seller
        'Q' -> CrossTrade
                   <$> getNumeric    9  -- Quantity
                   <*> getNumeric    6  -- Order Book
                   <*> getByteString 10 -- Cross Price
                   <*> getNumeric    9  -- Match Number
                   <*> getByteString 1  -- Cross Type
                   <*> getNumeric    10 -- Number of Trades
        'B' -> BrokenTrade
                   <$> getNumeric    9  -- Match Number
        'I' -> NOII
                   <$> getNumeric    9  -- Paired Quantity
                   <*> getNumeric    9  -- Imbalance Quantity
                   <*> getByteString 1  -- Imbalance Direction
                   <*> getNumeric    6  -- Order Book
                   <*> getByteString 10 -- Equilibrium Price
                   <*> getByteString 1  -- Cross Type
                   <*> getByteString 10 -- Best Bid Price
                   <*> getNumeric    9  -- Best Bid Quantity
                   <*> getByteString 10 -- Best Ask Price
                   <*> getNumeric    9  -- Best Ask Quantity
        _ -> return Unknown
