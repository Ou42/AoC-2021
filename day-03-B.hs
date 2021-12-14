{-
    2021-12-07
    . Advent of Code - Day 03 - Puzzle 2 of 2
      . given a file of binary #'s...
      . find the O2:   filtering L->R the most common bit
      . find the CO2:  filtering L->R the least common bit

      return (O2 * CO2) -- in decimal
-}

import Data.List

input = "Day-03-INPUT.txt"
-- input = "Day-03-INPUT-test-A-198-B-230.txt"

bin2dec :: [Int] -> Int
bin2dec binList = sum
                $ zipWith (*) input pow2
                  where
                    input = reverse binList
                    pow2  = map (2^) [0..]

toBinList :: [String] -> [[Int]]
toBinList = map (fmap (\m -> read [m] :: Int))

mostCommon :: Bool -> Int -> [[Int]] -> Int
mostCommon doMost index binList = func
                         $ onesCount
                         $ map (!! index) binList
  where
    most      = (\c -> if c*2 >= dataLen then 1 else 0)
    least     = (\c -> if c*2 <  dataLen then 1 else 0)
    func      = case doMost of
      True  -> most
      False -> least
    onesCount = (length . filter (== 1))
    dataLen   = length binList

filterBinListByIndex :: Bool -> [[Int]] -> Int -> [[Int]]
filterBinListByIndex doMost binList index = 
  case length binList of
    1 -> binList
    otherwise -> filter ((== match01) . (!! index)) binList
  where
    match01 = mostCommon doMost index binList

{-
    > :set prompt "> "
    > import Debug.Trace (traceShow)
    > foldl (\x y -> traceShow x $ (\a b -> a) x y) [[1],[2],[3]] [1..5]
    [[1],[2],[3]]
    [[1],[2],[3]]
    [[1],[2],[3]]
    [[1],[2],[3]]
    [[1],[2],[3]]
    [[1],[2],[3]]
    > foldl (\x y -> traceShow x $ (\a b -> [[b]]) x y) [[1],[2],[3]] [1..5]
    [[1],[2],[3]]
    [[1]]
    [[2]]
    [[3]]
    [[4]]
    [[5]]
-}

-- to test:
-- o2 = foldl (filterBinListByIndex True) binData [0..4]
-- [[1,0,1,1,1]] == 23 in decimal

main = do
  f <- readFile input
  -- let e = lines f
  -- print e -- ["00100","11110","10110","10111","10101","01111"
          -- ,"00111","11100","10000","11001","00010","01010"]

  let binData = toBinList . lines $ f
  print $ take 12 binData
      -- [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1]
      -- ,[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0]
      -- ,[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]]

  let binNumLen = length $ binData !! 0

  -- f <- readFile "Day-03-INPUT-test-A-198-B-230.txt"
  -- binData = toBinList . lines $ f
  let o2  = foldl (filterBinListByIndex True)  binData [0..(binNumLen-1)]
  let co2 = foldl (filterBinListByIndex False) binData [0..(binNumLen-1)]

  let o2Dec  = bin2dec $ head o2
  let co2Dec = bin2dec $ head co2

  putStrLn $ "Day 3 Part B - o2 = "
           ++ show o2Dec
           ++ " co2  = "
           ++ show co2Dec
  putStrLn $ "... the answer = o2 x co2Dec = "
           ++ show (o2Dec * co2Dec)
