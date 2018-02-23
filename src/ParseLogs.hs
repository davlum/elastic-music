{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module ParseLogs where

import           Data.Aeson
import qualified Data.Geohash    as GH
import qualified Data.Scientific as Sci
import qualified Data.Text       as T
import qualified Data.Vector     as V
import           GHC.Generics

newtype Lat = Lat {
  lat :: Double
} deriving (Eq, Ord, Show, Num)

newtype Lon = Lon {
  lon :: Double
} deriving (Eq, Ord, Show, Num)

data Coord = Coord Lat Lon deriving (Show, Generic)

instance FromJSON Coord where
  parseJSON (String s) = case GH.decode $ T.unpack s of
    Nothing     -> error "Couldn't decode string to (lat, lon)"
    Just (x, y) -> return $ Coord Lat{lat = x} Lon{lon = y}
  parseJSON _          = fail "Expected a string from the key 'key'"

newtype Hits = Hits {
  unHits :: Int
} deriving (Eq, Ord, Show, Num, Generic)

instance FromJSON Hits where
  parseJSON (Number x) = case Sci.toBoundedInteger x of
    Nothing -> error "Could not parse number from key 'doc_count' as Int"
    Just i  -> return Hits{unHits=i}
  parseJSON _          = fail "Expected a number from the key 'doc_count'"

data Bucket = Bucket {
  hits  :: Hits,
  coord :: Coord
} deriving (Show, Generic)

data Buckets = Buckets {
  buckets :: [Bucket],
  total   :: Hits
} deriving (Show, Generic)

instance FromJSON Bucket where
  parseJSON = withObject "Bucket" $ \v -> do
    docCount <- v .: "doc_count"
    hits <- parseJSON docCount
    key <- v .: "key"
    coord <- parseJSON key
    return Bucket{..}

instance FromJSON Buckets where
  parseJSON = withObject "Buckets" $ \v -> do
    aggregations <- v .: "aggregations"
    two <- aggregations .: "2"
    result <- two .: "buckets"
    buckets <- V.toList <$> mapM parseJSON result
    hits <- v .: "hits"
    total' <- hits .: "total"
    total <- parseJSON total'
    return Buckets{..}
