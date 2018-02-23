module Main where

import qualified Data.Aeson           as AE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char            as Char
import qualified ElasticMusic         as Mus
import           Euterpea
import qualified ParseLogs            as Logs
import qualified System.Random        as Rand

main :: IO ()
main = do
  putStrLn "Enter a seed:"
  char <- getChar
  if not $ Char.isDigit char
    then putStrLn " Come on just enter an Int man" >> main
    else do
      file <- BSL.readFile "src/out.json"
      let result = AE.eitherDecode file :: Either String [Logs.Buckets]
      case result of
        Left err -> fail err
        Right parsedJSON -> play music where
          seed = Char.digitToInt char
          randInts = Rand.randomRs (0,4) (Rand.mkStdGen seed)
          music = Mus.genLine randInts (head parsedJSON)
