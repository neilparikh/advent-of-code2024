module Util where

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'

wordsOn :: (Eq a) => a -> [a] -> [[a]]
wordsOn x = wordsBy (== x)

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _ = error "list does not have exactly 2 elems"

median :: [a] -> a
median xs
  | null xs = error "Can't find median of empty list"
  | even (length xs) = error "Can't find median of even length list"
  | otherwise = let
    l = length xs
    m = l `div` 2
    in xs !! m

converge :: (Eq a) => (a -> a) -> a -> a
converge f x = let x' = f x in (if x == x' then x else converge f x')
