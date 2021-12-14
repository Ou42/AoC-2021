{-
    2021-12-02
    . Advent of Code - Day 02 - Puzzle 2 of 2
      . given a file of directions and distances...
      . calculate the location of the submarine
      . now an additional factor is 'aim' used as follows:

          • down X increases your aim by X units.
          • up X decreases your aim by X units.
          • forward X does two things:
              • It increases your horizontal position by X units.
              • It increases your depth by your aim multiplied by X.

      . multiple the horizontal by the depth
-}

import Data.Maybe

input = "Day-02-INPUT.txt"
-- input = "Day-02-INPUT-test-A-150-B-900.txt"

moveWithAim :: (String, Int) -> (Int,Int,Int) -> (Int, Int, Int)
moveWithAim (dir, mag) (accDist, accDepth, accAim)
  | dir == "forward" = (accDist + mag, accDepth + accAim * mag, accAim)
  | dir == "up"      = (accDist, accDepth, accAim - mag)
  | dir == "down"    = (accDist, accDepth, accAim + mag)
--  | otherwise        = error "Direction: " ++ dir ++ " not valid."
  -- | otherwise        = Nothing
      -- • forward X does two things:
      --   • It increases your horizontal position by X units.
      --   • It increases your depth by your aim multiplied by X.

-- foldr (\(dir, mag) (accDist, accDepth, accAim) -> (accDist, accDepth, accAim)) ("hmm",0, 0) a
-- ("hmm",0,0)

-- foldr (moveWithAim) (0 ,0, 0) [(dir, mag)]

main = do
  d <- readFile input
  let e = lines d
  -- print e -- ["forward 5","down 5","forward 8","up 3","down 8","forward 2"]

  let f = map (break (== ' ')) e
  -- print f -- [("forward"," 5"),("down"," 5"),("forward"," 8"),("up"," 3"),("down"," 8"),("forward"," 2")]

  let g = map (fmap (\m -> read m :: Int)) f
  print g -- [("forward",5),("down",5),("forward",8),("up",3),("down",8),("forward",2)]

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