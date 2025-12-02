import Data.Text qualified as T

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) ((splitter ls) (:) [])
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

toPair :: [a] -> (a, a)
toPair (x : y : []) = (x, y)

range :: Int -> Int -> [Int]
range x y = [x .. y]

valid1 :: String -> Bool
valid1 x
  | uncurry (==) $ splitAt ((length x) `div` 2) x = False
  | otherwise = True

valid2 :: String -> Bool
valid2 x
  | any allEqual $ [chunksOf i x | i <- [1 .. ((length x) - 1)], (length x) `mod` i == 0] = False
  | otherwise = True

-- "but this is like O(n^1000)!!"
-- cry more, just get a better computer, idiot
solve :: (String -> Bool) -> String -> String
solve valid =
  show
    . foldl (+) 0
    . map read
    . filter (not . valid)
    . map show
    . concatMap (uncurry range . toPair . map (read . T.unpack) . T.split (== '-'))
    . T.split (== ',')
    . T.strip
    . T.pack

solve1 :: String -> String
solve1 = solve valid1

solve2 :: String -> String
solve2 = solve valid2
