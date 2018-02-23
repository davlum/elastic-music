{-# LANGUAGE ApplicativeDo #-}

module ElasticMusic where

import qualified Data.MarkovChain as M
import           Euterpea
import qualified ParseLogs        as Logs


-- Notes map to one pentatonic scale for now
mapInts :: Int -> PitchClass
mapInts x = case x of
              0 -> C
              1 -> D
              2 -> E
              3 -> G
              4 -> A
              _ -> Bf


-- Fairly arbitrary mapping. Would like to gets some
-- stats to support this.
latToOctave :: Logs.Lat -> Int
latToOctave x
  | x >= 50 = 5
  | x >= 40 && x < 50 = 4
  | x > 25 && x < 40 = 3
  | x <= 25 && x > 0 = 2
  | x <= 0 = 1
  | otherwise = 3

-- Fairly arbitrary mapping. Would like to gets some
-- stats to support this.
hitsToDurs :: Logs.Hits -> Logs.Hits -> Dur
hitsToDurs total unique
    | ratio >= 0.25 = en
    | ratio >= 0.10 && ratio < 0.25 = qn
    | ratio >= 0.5 && ratio < 0.10 = hn
    | ratio >= 0.025 && ratio < 0.5 = wn
    | otherwise = dwn
    where
      total' = fromIntegral $ Logs.unHits total
      unique' = fromIntegral $ Logs.unHits unique
      ratio = unique' / total' :: Double

toLine :: [Int] -> Dur -> Octave -> (Music Pitch, [Int])
toLine randInts duration o =
  let (ints, r) = splitAt (floor $ 4.0 / fromRational duration) randInts
      pitchClasses = fmap mapInts ints
      applyPitch :: PitchClass -> Music Pitch
      applyPitch p = note duration (p, o)
   in (line $ fmap applyPitch pitchClasses, r)


genLine :: [Int] -> Logs.Buckets -> Music Pitch
genLine randInts buckets = go randInts durOcts
  where
    go _ [] = rest 0
    go rands ((dura,oct):dos) =
      let (mus, remaining) = toLine rands dura oct
       in mus :=: go remaining dos
    bucketls = Logs.buckets buckets
    total = Logs.total buckets
    durs = fmap (hitsToDurs total . Logs.hits) bucketls
    octaves = fmap (latToOctave . (\(Logs.Coord x _) -> x) . Logs.coord) bucketls
    durOcts = zip durs octaves
