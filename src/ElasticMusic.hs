--{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ElasticMusic where

import           Data.Aeson
import qualified Data.Geohash     as GH
import qualified Data.MarkovChain as M
import           Data.Maybe
import qualified Data.Vector      as V
import           Euterpea         hiding (key)
import           GHC.Generics

data Bucket = Bucket {
 doc_count :: Int,
 key       :: String
} deriving (Show, Generic)


data Buckets = Buckets {
  buckets :: [Bucket],
  total   :: Int
  } deriving (Show, Generic)

instance FromJSON Bucket

{-
 - Use applicative do once I can get cabal to find
 - ghc 8.*
 -}
instance FromJSON Buckets where
  parseJSON = withObject "Buckets" $ \v -> do
    aggregations <- v .: "aggregations"
    two <- aggregations .: "2"
    result <- two .: "buckets"
    buckets <- V.toList <$> mapM parseJSON result
    hits <- v .: "hits"
    total <- hits .: "total"
    return Buckets{..}

{-
 - Notes map to one pentatonic scale for now
 -}
mapInts :: Int -> PitchClass
mapInts x = case x of
              0 -> C
              1 -> D
              2 -> E
              3 -> G
              4 -> A
              _ -> Bf

{-
 - Fairly arbitrary mapping. Would like to gets some
 - stats to support this.
 -}
latToOctave :: Double -> Int
latToOctave x
  | x >= 50 = 5
  | x >= 40 && x < 50 = 4
  | x > 25 && x < 40 = 3
  | x <= 25 && x > 0 = 2
  | x <= 0 = 1
  | otherwise = 3

{-
 - Fairly arbitrary mapping. Would like to gets some
 - stats to support this.
 -}
hitsToDurs :: (Ord a, Fractional a) => Int -> Int -> Dur
hitsToDurs total unique
    | ratio >= 0.25 = en
    | ratio >= 0.10 && ratio < 0.25 = qn
    | ratio >= 0.5 && ratio < 0.10 = hn
    | ratio >= 0.025 && ratio < 0.5 = wn
    | ratio < 0.025 = dwn
    where
      ratio = fromIntegral unique/fromIntegral total

toLine :: [Int] -> Dur -> Octave -> (Music Pitch, [Int])
toLine randInts d o =
  let (ints, rest) = splitAt (floor $ 4.0/fromRational d) randInts
      pitchClasses = fmap mapInts ints
      applyPitch :: PitchClass -> Music Pitch
      applyPitch p = note d (p, o)
   in (line $ fmap applyPitch pitchClasses, rest)

genLine :: [Int] -> Buckets -> Music Pitch
genLine randInts hits = go randInts durOcts
  where
    go _ [] = rest 0
    go rands ((d,o):dos) =
      let (mus, rest) = toLine rands d o
       in mus :=: go rest dos
    hitLs = buckets hits
    tot = total hits
    durs = fmap ((hitsToDurs tot) . doc_count) hitLs
    octaves = fmap (latToOctave . fst . fromMaybe (45,45) . GH.decode . key) hitLs
    durOcts = zip durs octaves
