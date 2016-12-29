module Lib where

import Data.Csv
import Data.Time

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector as V


parseDay :: Monad m => String -> m Day
parseDay = parseTimeM True defaultTimeLocale "%F"

instance FromField Day where
  parseField = parseDay . B.unpack

type Close = (Day, Double)
type Value = (Day, Double)
type Rec = (Day, Double, Double, Double, Double, Double, Double)

decodeClose :: L.ByteString -> Either String (V.Vector Close)
decodeClose bs = (V.reverse . V.map f) <$> decode HasHeader bs
  where
    f :: Rec -> Close
    f (date,open,high,low,close,volume,adj_close) = (date,close)

decodeClose' :: FilePath -> IO (V.Vector Close)
decodeClose' csv = do
  x <- decodeClose <$> L.readFile csv
  case x of
    Left err -> fail err
    Right closes -> return closes

invest1FromMonthly :: Day -> V.Vector Close -> Double
invest1FromMonthly start
  = snd
  . V.foldl f (1, 0)
  . V.map snd
  . V.dropWhile ((< start) . fst)
  where
    f (close', value) close = (close, value * close / close' + 1)

investFromMonthly :: Double -> Day -> V.Vector Close -> Double
investFromMonthly amount start = (* amount) . invest1FromMonthly start
