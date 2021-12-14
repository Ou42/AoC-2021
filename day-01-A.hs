{-
    2021-12-01
    . Advent of Code - Day 01 - Puzzle 1 of 2
      . given a file of numbers, find # of times a # in the file is > the prev #
-}

input = "Day-01-INPUT.txt"

isBigger :: [Int] -> [Int]
isBigger [] = []
isBigger (x:[]) = []
isBigger (x1:x2:xs) = if x2 > x1
                        then 1 : isBigger (x2:xs)
                        else isBigger (x2:xs)

main = do
  d <- readFile input
  let e = lines d
  -- print e

  let f = map (\i -> read i :: Int) e
  print f

  let g = isBigger f
  print g

  -- print $ "The number of times a depth is greater than a previous depth = " ++ show $ sum g
  print $ "The number of times a depth is greater than a previous depth = "
  print $ show $ sum g