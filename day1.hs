import Data.List (transpose, sort)
import Data.Bifunctor

main = do
    input <- fmap (listToTuple . transpose . fmap (fmap read . words) . lines) getContents
    print $ part1 input
    print $ part2 input

part1 :: ([Int], [Int]) -> Int
part1 = sum . uncurry (zipWith (\x y -> abs $ x - y)) . bimap sort sort

part2 :: ([Int], [Int]) -> Int
part2 (left, right) = sum $ zipWith (*) left (fmap (flip count right) left)

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _ = error "list does not have exactly 2 elems"

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
