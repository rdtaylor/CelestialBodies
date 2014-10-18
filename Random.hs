module Random where

import System.Random

type Seed = Integer

data RandomWithSeed = RandomWithSeed {
  value :: Integer
, seed :: Seed
} deriving (Show)

getInitSeed :: Integer
getInitSeed = 19000912 -- TODO -- Base off of timestamp or something
                       --         more dynamic

getSeed :: (Seed, StdGen) -> Integer
getSeed n = fromIntegral $ read (takeWhile (\x -> x /= ' ') $ show $ snd n)

getRandom :: Seed -> (Integer, Integer) -> RandomWithSeed
getRandom in_seed (min_num, max_num) = RandomWithSeed {
  value = fst rand_num
, seed = (getSeed rand_num)
} where rand_num = randomR (min_num, max_num)
                           (mkStdGen $ fromIntegral in_seed)
