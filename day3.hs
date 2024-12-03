import Data.Char (isDigit)
import Debug.Trace (traceId)

main = do
    input <- getContents
    print $ part1 input Start 0

data State = Start | M | U | L | LParen | Num1 String | Comma String | Num2 String String

part1 :: String -> State -> Int -> Int
part1 "" _ acc = acc
part1 ('m':xs) Start acc = part1 xs M acc
part1 ('u':xs) M acc = part1 xs U acc
part1 ('l':xs) U acc = part1 xs L acc
part1 ('(':xs) L acc = part1 xs LParen acc
part1 (x:xs) LParen acc
    | isDigit x = part1 xs (Num1 [x]) acc
    | otherwise = part1 xs Start acc
part1 (',':xs) (Num1 y) acc = part1 xs (Comma y) acc
part1 (x:xs) (Num1 y) acc
    | isDigit x && length y < 3 = part1 xs (Num1 (x:y)) acc
    | otherwise = part1 xs Start acc
part1 (x:xs) (Comma other) acc
    | isDigit x = part1 xs (Num2 [x] other) acc
    | otherwise = part1 xs Start acc
part1 (')':xs) (Num2 a b) acc = part1 xs Start (acc + ((read $ reverse a) *  (read $ reverse b)))
part1 (x:xs) (Num2 y other) acc
    | isDigit x = part1 xs (Num2 (x:y) other) acc
    | otherwise = part1 xs Start acc
part1 (_:xs) _ acc = part1 xs Start acc
