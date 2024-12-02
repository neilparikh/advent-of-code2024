main = do
    input <- fmap (fmap (fmap read . words) . lines) getContents
    print $ part1 input
    print $ part2 input

part1 :: [[Int]] -> Int
part1 = length . filter safe

part2 :: [[Int]] -> Int
part2 = length . filter (any safe) . fmap withOneDropped
    where
    withOneDropped :: [Int] -> [[Int]]
    withOneDropped xs = fmap (without xs) [0..(length xs - 1)]

safe :: [Int] -> Bool
safe xs = let diffs = zipWith subtract xs (tail xs) in (all (> 0) diffs || all (< 0) diffs) && all ((<= 3) . abs) diffs

without :: [Int] -> Int -> [Int]
without xs i = take i xs ++ drop (i + 1) xs
