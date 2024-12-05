import Data.List
import Data.Bifunctor (bimap)
import Util

main = do
    (rules, manuals) <- fmap (parse) getContents
    print $ part1 manuals rules
    print $ part2 manuals rules

part1 :: [[String]] -> [(String, String)] -> Int
part1 manuals rules = sum . fmap (read . median) $ filter (validateManual rules) manuals

part2 :: [[String]] -> [(String, String)] -> Int
part2 manuals rules = sum . fmap (read . median) $ fmap (converge (fixManual rules)) . filter (not . validateManual rules) $ manuals

parse :: String -> ([(String, String)], [[String]])
parse = bimap (fmap (listToTuple . wordsOn '|')) (fmap (wordsOn ',')) . listToTuple . wordsOn "" . lines

validateManual :: [(String, String)] -> [String] -> Bool
validateManual rules [] = True
validateManual rules [_] = True
validateManual rules (a:b:xs)
    | (b, a) `elem` rules = False
    | otherwise = validateManual rules (b:xs)

fixManual :: [(String, String)] -> [String] -> [String]
fixManual rules [] = []
fixManual rules [x] = [x]
fixManual rules (a:b:xs)
    | (b, a) `elem` rules = b:(fixManual rules (a:xs))
    | otherwise = a:(fixManual rules (b:xs))
