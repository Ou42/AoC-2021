{-
    2021-12-01
    . Advent of Code - Day 01 - Puzzle 2 of 2
      . given a file of numbers...
      . sum each group of 3 #'s starting at index 0 and inc by 1
      . find # of times each succ sum is > the prev sum
-}

input = "Day-01-INPUT.txt"
-- input = "Day-01-INPUT-test-A-7-B-5.txt"

isBigger :: [Int] -> [Int]
isBigger [] = []
isBigger (x:[]) = []
isBigger (x1:x2:xs) = if x2 > x1
                        then 1 : isBigger (x2:xs)
                        else isBigger (x2:xs)

sum3 :: [Int] -> [Int]
sum3 (x1:x2:x3:xs) = (x1 + x2 + x3) : sum3 (x2:x3:xs)
sum3 _ = []

main = do
  d <- readFile input
  let e = lines d
  -- print e

  let f = map (\i -> read i :: Int) e
  print f

  let g = sum3 f
  print g

  let h = isBigger g
  print h

  -- print $ "The number of times a depth is greater than a previous depth = " ++ show $ sum g
  print $ "The number of times a 3 group depth is greater than a previous 3 group depth = "
  print $ show $ sum h