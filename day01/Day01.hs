sign :: Int -> Int
sign x
  | x == 0 = 0
  | x < 0 = -1
  | x > 0 = 1

update1 :: (Int, Int) -> Int -> (Int, Int)
update1 (oldRot, oldNum) addRot =
  let newRot = (oldRot + addRot) `mod` 100
   in case (oldRot, newRot) of
        (_, 0) -> (newRot, oldNum + 1)
        _ -> (newRot, oldNum)

-- Holy fuck lol...
update2 :: (Int, Int) -> Int -> (Int, Int)
update2 s addRot =
  foldl update1 s $ replicate (abs addRot) $ sign addRot

parseRotation :: String -> Int
parseRotation ('L' : num) = -read num
parseRotation ('R' : num) = read num
parseRotation _ = undefined

solve :: ((Int, Int) -> Int -> (Int, Int)) -> String -> String
solve update =
  show
    . snd
    . foldl update (50, 0)
    . map parseRotation
    . filter ((> 0) . length)
    . lines

solve1 :: String -> String
solve1 = solve update1

solve2 :: String -> String
solve2 = solve update2
