import Util
import qualified Data.Set as S

main = do
    grid <- fmap lines getContents
    let init = Pos (findPos grid) (0, -1)
    let (_, ans) = converge (step grid) (init, S.insert init S.empty)
    print $ S.size $ S.map xy ans
    print $ part2 grid init
    return ()

-- x, y and direction
data Pos = Pos { xy :: (Int, Int), delta :: (Int, Int) } deriving (Show, Eq, Ord)

step :: [[Char]] -> (Pos, S.Set Pos) -> (Pos, S.Set Pos)
step grid state@((Pos (x, y) (dx, dy)), seen)
    | x + dx < 0 || x + dx >= length (head grid) = state
    | y + dy < 0 || y + dy >= length grid = state
    | (grid !! (y + dy)) !! (x + dx) == '#' = (Pos (x, y) (rotate (dx, dy)), S.insert (Pos (x, y) (rotate (dx, dy))) seen)
    | otherwise = (Pos (x + dx, y + dy) (dx, dy), S.insert (Pos (x + dx, y + dy) (dx, dy)) seen)

part2 :: [[Char]] -> Pos -> S.Set (Int, Int)
part2 grid init = S.fromList $ filter inGrid $ fmap canBlock allStates
    where
    inGrid (x, y) = x > 0 && x < length (head grid) && y > 0 && y < length grid
    allStates = untilDup $ iterate (step grid) (init, S.insert init S.empty)
    untilDup (x:y:xs) = if (x == y) then [x] else (x:(untilDup (y:xs)))

canBlock :: (Pos, S.Set Pos) -> (Int, Int)
canBlock (Pos i@(x, y) j@(dx, dy), seen) = if any (`S.member` seen) next10 then (move i j) else (-1, -1)
    where
    next10 = fmap (\z -> Pos z (rotate j)) $ fmap (\n -> applyNTimes n (flip move (rotate j)) i) [1..10]

move (x, y) (dx, dy) = (x + dx, y + dy)

rotate :: (Int, Int) -> (Int, Int)
rotate (0, 1) = (-1, 0)
rotate (0, -1) = (1, 0)
rotate (1, 0) = (0, 1)
rotate (-1, 0) = (0, -1)

findPos :: [[Char]] -> (Int, Int)
findPos grid = (x, y)
    where
    y = length . takeWhile (not . ('^' `elem`)) $ grid
    x = length . takeWhile (not . ('^' ==)) . head . filter ('^' `elem`) $ grid
