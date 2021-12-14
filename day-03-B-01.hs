{-
    2021-12-07
    . Advent of Code - Day 03 - Puzzle 2 of 2
      . given a file of binary #'s...
      . find the O2:   filtering L->R the most common bit
      . find the CO2:  filtering L->R the least common bit

      return (O2 * CO2) -- in decimal
-}

import Data.List

-- input = "Day-03-INPUT.txt"
input = "Day-03-INPUT-test-A-198-B-230.txt"

bin2dec :: [Int] -> Int
bin2dec binList = sum
                $ zipWith (*) input pow2
                  where
                    input = reverse binList
                    pow2  = map (2^) [0..]

toBinList :: [String] -> [[Int]]
toBinList = map (fmap (\m -> read [m] :: Int))

mostCommon :: Int -> [[Int]] -> Int
mostCommon index binList = most
                         $ onesCount
                         $ map (!! index) binList
  where
    most      = (\c -> if c*2 >= dataLen then 1 else 0)
    onesCount = (length . filter (== 1))
    dataLen   = length binList

filterBinListByIndex :: Int -> [[Int]] -> [[Int]]
filterBinListByIndex index binList = filter ((== match01) . (!! index)) binList
  where
    match01 = mostCommon index binList

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
{-
filterBinList :: [[Int]] -> [Int]
filterBinList binList = fbi index
  where fbi
  filterBinListByIndex index
  where
    binNumLen = length $ binList !! 0
    indicies = [0..(binNumLen - 1)]
-}

main = do
  d <- readFile input
  let e = lines d
  -- print e -- ["00100","11110","10110","10111","10101","01111"
          -- ,"00111","11100","10000","11001","00010","01010"]

  let dataLen = length e
  putStrLn $ "dataLen = " ++ show dataLen

  let binNumLen = length $ e !! 0

--   let f = map (fmap (\m -> read [m] :: Int)) e
  let f = toBinList e
  -- print f -- [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1]
          -- ,[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0]
          -- ,[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]]

  -- from: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html
  {-  
      transpose :: [[a]] -> [[a]]

      The transpose function transposes the rows and columns of its argument. For example,

      >>> transpose [[1,2,3],[4,5,6]]
      [[1,4],[2,5],[3,6]]
  -}
  let g = transpose f
  -- print g -- [[0,1,1,1,1,0,0,1,1,1,0,0],[0,1,0,0,0,1,0,1,0,1,0,1]
          -- ,[1,1,1,1,1,1,1,1,0,0,0,0],[0,1,1,1,0,1,1,0,0,0,1,1]
          -- ,[0,0,0,1,1,1,1,0,0,1,0,0]]

  let onesCount = map (length . filter (== 1)) g
  -- print onesCount -- [7,5,8,7,5]

  -- *** how to do Int division?!
  --        > :t div
  --        div :: Integral a => a -> a -> a
  --        div 5 2
  --        2
  let gammaBin = map (\c -> if c*2 > dataLen then 1 else 0) onesCount
  print gammaBin -- [1,0,1,1,0]

  -- tried to do it Lazily, but this never finishes... reverse issue?!
  -- zipWith (*) [1,0,1,1,0] (reverse $ map (2^) [0..])

  let gamma = bin2dec gammaBin
  print gamma -- 22

  let epsilon = bin2dec $ map (1-) gammaBin
  print epsilon -- 9

  putStrLn $ "Day 3 Part A - gamma = "
           ++ show gamma
           ++ " epsilon = "
           ++ show epsilon
  putStrLn $ "... the answer = gamma x epsilon = "
           ++ show (gamma * epsilon)
          --  ++ show $ gamma * epsilon

  -- error "Stop here!"

  {-

  let g = map (break (== ' ')) f
  -- print f -- [("forward"," 5"),("down"," 5"),("forward"," 8"),("up"," 3"),("down"," 8"),("forward"," 2")]

  --
  -- how to compose the funcs in the 2 maps in f & g?!
  --

  -- foldr (moveWithAim) (0 ,0, 0) [(dir, mag)]

  -- let h = foldr (moveWithAim) (0, 0, 0) g
  -- above prints (15,90,10) -- see below!
  let h = foldr (moveWithAim) (0, 0, 0) $ reverse g
  print h -- (15,60,10)

  {-
      > a
      [("forward",5),("down",5),("forward",8),("up",3),("down",8),("forward",2)]
      > foldr (moveWithAim) (0, 0, 0) [a !! 0]
      (5,0,0)
      > foldr (moveWithAim) (5, 0, 0) [a !! 1]
      (5,0,5)
      > foldr (moveWithAim) (5, 0, 5) [a !! 2]
      (13,40,5)
      > foldr (moveWithAim) (13, 40, 5) [a !! 3]
      (13,40,2)
      > foldr (moveWithAim) (13, 40, 2) [a !! 4]
      (13,40,10)
      > foldr (moveWithAim) (13, 40, 10) [a !! 5]
      (15,60,10)
      > 15*60
      900

      > main
      [("forward",5),("down",5),("forward",8),("up",3),("down",8),("forward",2)]
      (15,90,10)

      ?????!!!!!

      *** SOLVED! *** foldr is not L -> R but "starting from the Right"!
      ... need to reverse the list!
      let h = foldr (moveWithAim) (0, 0, 0) $ reverse g

  -}

  let l = (\(f, s, _) -> f * s) h

  putStrLn $ "The product of the change in directions is:"
  putStrLn $ show l
  
  -}