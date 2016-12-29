module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [amount, start, csv] <- getArgs
  value <- investFromMonthly (read amount) <$> parseDay start <*> decodeClose' csv
  putStrLn $ "Investing "       ++ amount
          ++ " in OMX30 since " ++ start
          ++ " would yield "    ++ show value
