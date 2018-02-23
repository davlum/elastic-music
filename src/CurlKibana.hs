{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module CurlKibana where

import qualified Control.Concurrent         as Conc
import qualified Control.Concurrent.STM     as STM
import qualified Data.Aeson                 as AE
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as Text
import qualified Data.Time.Clock.POSIX      as Time
import           NeatInterpolation          (text)
import qualified Network.Curl               as Curl
import qualified ParseLogs                  as Logs

newtype Milliseconds = Milliseconds {
  ms :: Int
} deriving (Eq, Ord, Show, Num)

url :: String
url = "https://search-fastly-web-prod-iofkdekqn6valdra4mckpoqj3e.us-east-1.es.amazonaws.com/_plugin/kibana/api/console/proxy?uri=_search"

-- Post to Elastic search
postBody :: Milliseconds -> Milliseconds -> String
postBody start end =
  let start' = (Text.pack . show . ms) start
      end' = (Text.pack . show . ms) end
   in Text.unpack [text|
  {
  "query": {
    "bool": {
      "must": [
        {
          "query_string": {
            "query": "*",
            "analyze_wildcard": true
          }
        },
        {
          "range": {
            "time": {
              "gte": $start',
              "lte": $end',
              "format": "epoch_millis"
            }
          }
        }
      ],
      "must_not": []
    }
  },
  "size": 0,
  "_source": {
    "excludes": []
  },
  "aggs": {
    "2": {
      "geohash_grid": {
        "field": "req.geoPoint",
        "precision": 2
      }
    }
  }
}
|]


-- Get Current time in milliseconds
getCurTime :: IO Milliseconds
getCurTime = do
  curTime <- Time.getPOSIXTime
  return Milliseconds{ms=floor $ curTime * 1000}

curlOnce :: Milliseconds -> Milliseconds -> IO Logs.Buckets
curlOnce start end = do
  response <- Curl.curlGetString
      url
      [Curl.CurlHttpHeaders
          ["kbn-xsrf: reporting", "content-type: application/json"]
     , Curl.CurlPostFields
          [postBody start end]]
  let (code, body) = response
  case code of
    Curl.CurlOK -> let parsedJSON = AE.eitherDecode
                        (BSL.pack body) :: Either String Logs.Buckets
              in case parsedJSON of
                Left err   -> fail err
                Right json -> return json
    _ -> error body

tenSecs, fiveSecs :: Milliseconds
tenSecs = 10000
fiveSecs = 5000

delayTime :: Int
delayTime = 4999000

writeCurlThread :: STM.TChan Logs.Buckets -> IO ()
writeCurlThread chan = do
  curTime <- getCurTime
  let tenSecsAgo = curTime - tenSecs
      fiveSecsAgo = curTime - fiveSecs
  json <- curlOnce tenSecsAgo fiveSecsAgo
  STM.atomically $ STM.writeTChan chan json
  Conc.threadDelay delayTime
  writeCurlThread chan

readCurlThread :: STM.TChan Logs.Buckets -> IO ()
readCurlThread chan = do
  newRequest <- STM.atomically $ STM.readTChan chan
  print newRequest
  readCurlThread chan

main :: IO ()
main = do
  chan <- STM.atomically STM.newTChan
  wt <- Conc.forkIO $ writeCurlThread chan
  rt <- Conc.forkIO $ readCurlThread chan
  Conc.threadDelay $ delayTime * 5
  Conc.killThread wt
  Conc.killThread rt
