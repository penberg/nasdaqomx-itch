-- |
-- Module      : NASDAQOMX.Nordic.ITCH
-- Copyright   : (c) Pekka Enberg, 2014
-- License     : BSD-style
--
-- A Haskell implementation of NASDAQ OMX Norid ITCH protocol as specified in:
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

import qualified Data.ByteString.Internal as BS (w2c)
import Control.Applicative
import Data.ByteString
import Data.Binary.Get
import Data.Word

type Numeric      = ByteString
type Alphanumeric = ByteString
type Alphabetic   = ByteString

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
        , price                     :: Numeric }
    | AddOrderMPID
        { orderReferenceNumber      :: Numeric
        , buySellIndicator          :: Alphabetic
        , quantity                  :: Numeric
        , orderBook                 :: Numeric
        , price                     :: Numeric
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
        , tradePrice                :: Numeric
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
        , tradePrice                :: Numeric
        , buyerParticipantID        :: Alphabetic
        , sellerParticipantID       :: Alphabetic }
    | CrossTrade
        { quantity                  :: Numeric
        , orderBook                 :: Numeric
        , crossPrice                :: Numeric
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
        , equilibriumPrice          :: Numeric
        , crossType                 :: Alphabetic
        , bestBidPrice              :: Numeric
        , bestBidQuantity           :: Numeric
        , bestAskPrice              :: Numeric
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

parseMessage :: Word8 -> Get Message
parseMessage msgType = do
    case (BS.w2c msgType) of
        'T' -> Seconds
                   <$> getByteString 5  -- Second
        'M' -> Milliseconds
                   <$> getByteString 3  -- Millisecond
        'S' -> SystemEvent
                   <$> getByteString 1  -- Event Code
        'O' -> MarketSegmentState
                   <$> getByteString 3  -- Market Segment ID
                   <*> getByteString 1  -- Event Code
        'R' -> OrderBookDirectory
                   <$> getByteString 6  -- Order Book
                   <*> getByteString 16 -- Symbol
                   <*> getByteString 12 -- ISIN
                   <*> getByteString 3  -- Financial Product
                   <*> getByteString 3  -- Trading Currency
                   <*> getByteString 4  -- MIC
                   <*> getByteString 3  -- Market Segment ID
                   <*> getByteString 8  -- Note Codes
                   <*> getByteString 9  -- Round Lot Size
        'H' -> OrderBookTradingAction
                   <$> getByteString 6  -- Order Book
                   <*> getByteString 1  -- Trading State
                   <*> getByteString 1  -- Reserved
                   <*> getByteString 4  -- Reason
        'A' -> AddOrder
                   <$> getByteString 9  -- Order Reference Number
                   <*> getByteString 1  -- Buy/Sell Indicator
                   <*> getByteString 9  -- Quantity
                   <*> getByteString 6  -- Order Book
                   <*> getByteString 10 -- Price
        'F' -> AddOrderMPID
                   <$> getByteString 9  -- Order Reference Number
                   <*> getByteString 1  -- Buy/Sell Indicator
                   <*> getByteString 9  -- Quantity
                   <*> getByteString 6  -- Order Book
                   <*> getByteString 10 -- Price
                   <*> getByteString 4  -- Attribution
        'E' ->  OrderExecuted
                   <$> getByteString 9  -- Order Reference Number
                   <*> getByteString 9  -- Executed Quantity
                   <*> getByteString 9  -- Match Number
                   <*> getByteString 4  -- Participant ID, owner
                   <*> getByteString 4  -- Participant ID, counterparty
        'C' -> OrderExecutedWithPrice
                   <$> getByteString 9  -- Order Reference Number
                   <*> getByteString 9  -- Executed Quantity
                   <*> getByteString 9  -- Match Number
                   <*> getByteString 1  -- Printable
                   <*> getByteString 10 -- Trade Price
                   <*> getByteString 4  -- Participant ID, owner
                   <*> getByteString 4  -- Participant ID, counterparty
        'X' -> OrderCancel
                   <$> getByteString 9  -- Order Reference Number
                   <*> getByteString 9  -- Canceled Quantity
        'D' -> OrderDelete
                   <$> getByteString 9  -- Order Reference Number
        'P' -> Trade
                   <$> getByteString 9  -- Order Reference Number
                   <*> getByteString 1  -- Trade Type
                   <*> getByteString 9  -- Quantity
                   <*> getByteString 6  -- Order Book
                   <*> getByteString 9  -- Match Number
                   <*> getByteString 10 -- Trade Price
                   <*> getByteString 4  -- Participant ID, buyer
                   <*> getByteString 4  -- Participant ID, seller
        'Q' -> CrossTrade
                   <$> getByteString 9  -- Quantity
                   <*> getByteString 6  -- Order Book
                   <*> getByteString 10 -- Cross Price
                   <*> getByteString 9  -- Match Number
                   <*> getByteString 1  -- Cross Type
                   <*> getByteString 10 -- Number of Trades
        'B' -> BrokenTrade
                   <$> getByteString 9  -- Match Number
        'I' -> NOII
                   <$> getByteString 9  -- Paired Quantity
                   <*> getByteString 9  -- Imbalance Quantity
                   <*> getByteString 1  -- Imbalance Direction
                   <*> getByteString 6  -- Order Book
                   <*> getByteString 10 -- Equilibrium Price
                   <*> getByteString 1  -- Cross Type
                   <*> getByteString 10 -- Best Bid Price
                   <*> getByteString 9  -- Best Bid Quantity
                   <*> getByteString 10 -- Best Ask Price
                   <*> getByteString 9  -- Best Ask Quantity
        _ -> return $! Unknown
