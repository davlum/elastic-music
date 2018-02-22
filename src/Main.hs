module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import qualified Data.Geohash         as GH
import           ElasticMusic
import           Euterpea
import           System.Random

main :: IO ()
main = do
  putStrLn "Enter a seed:"
  char <- getChar
  if not $ isDigit char
    then putStrLn " Come on just enter an Int man" >> main
    else do
      file <- BSL.readFile "src/out.json"
      let result = eitherDecode file :: Either String [Buckets]
      case result of
        Left err -> fail err
        Right buckets -> play music where
          seed = digitToInt char
          randInts = randomRs (0,4) (mkStdGen seed)
          music = genLine randInts (head buckets)
