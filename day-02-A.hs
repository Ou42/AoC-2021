{-
    2021-12-02
    . Advent of Code - Day 02 - Puzzle 1 of 2
      . given a file of directions and distances...
      . calculate the location of the submarine
      . multiple the horizontal by the depth
-}

import Data.Maybe

-- input = "Day-02-INPUT.txt"
input = "Day-02-INPUT-test-A-150-B-900.txt"

-- move :: (String, Int) -> (Int, Int)
move :: (String, Int) -> Maybe (Int, Int)
move (dir, mag)
  | dir == "forward" = Just (mag, 0)
  | dir == "up"      = Just (0, (-mag))
  | dir == "down"    = Just (0, mag)
--  | otherwise        = error "Direction: " ++ dir ++ " not valid."
  | otherwise        = Nothing

main = do
  d <- readFile input
  let e = lines d
  -- print e -- ["forward 5","down 5","forward 8","up 3","down 8","forward 2"]

  let f = map (break (== ' ')) e
  -- print f -- [("forward"," 5"),("down"," 5"),("forward"," 8"),("up"," 3"),("down"," 8"),("forward"," 2")]

  let g = map (fmap (\m -> read m :: Int)) f
  -- print g -- [("forward",5),("down",5),("forward",8),("up",3),("down",8),("forward",2)]

  --
  -- how to compose the funcs in the 2 maps in f & g?!
  --

  let h = map move g
  -- print h -- [Just (5,0),Just (0,5),Just (8,0),Just (0,-3),Just (0,8),Just (2,0)]

  {-
      
    h = [Just (5,0),Just (0,5),Just (8,0),Just (0,-3),Just (0,8),Just (2,0)]
    *Main Text.Show.Functions Data.Maybe> unzip h

    <interactive>:67:7: error:
        * Couldn't match type `Maybe (Integer, Integer)' with `(a, b)'
          Expected type: [(a, b)]
            Actual type: [Maybe (Integer, Integer)]
        * In the first argument of `unzip', namely `a'
          In the expression: unzip a
          In an equation for `it': it = unzip a
        * Relevant bindings include
            it :: ([a], [b]) (bound at <interactive>:67:1)  
    
  -}

  let i = catMaybes h
  -- print i -- [(5,0),(0,5),(8,0),(0,-3),(0,8),(2,0)]

  -- instead of catMaybes ... how to fold or Applicative sum
  -- a List of Maybe (Int, Int) ? Not fst+snd, but (sum fst's, sum snd's)

  let j = unzip i
  -- print j -- ([5,0,8,0,0,2],[0,5,0,-3,8,0])

  let k = (\(f, s) -> (sum f, sum s)) j
  -- print k -- (15,10)

  let l = (\(f, s) -> f * s) k
  -- 150

  putStrLn $ "The product of the change in directions is:"
  putStrLn $ show l