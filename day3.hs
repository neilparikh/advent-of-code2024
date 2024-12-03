import Data.Char (isDigit)

main = do
    input <- getContents
    print $ part1 input Start 0
    print $ part2 input Start True 0

data Method = Mul | Do | Dont
data State = Start | M | U | L | LParen Method | Num1 String | Comma String | Num2 String String | D | O | N | Quote | T

part1 :: String -> State -> Int -> Int
part1 "" _ acc = acc
part1 ('m':xs) Start acc = part1 xs M acc
part1 ('u':xs) M acc = part1 xs U acc
part1 ('l':xs) U acc = part1 xs L acc
part1 ('(':xs) L acc = part1 xs (LParen Mul) acc
part1 (x:xs) (LParen Mul) acc
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

part2 :: String -> State -> Bool -> Int -> Int
part2 "" _ _ acc = acc
part2 ('m':xs) Start mulEnabled acc = part2 xs M mulEnabled acc
part2 ('u':xs) M mulEnabled acc = part2 xs U mulEnabled acc
part2 ('l':xs) U mulEnabled acc = part2 xs L mulEnabled acc
part2 ('d':xs) Start mulEnabled acc = part2 xs D mulEnabled acc
part2 ('o':xs) D mulEnabled acc = part2 xs O mulEnabled acc
part2 ('n':xs) O mulEnabled acc = part2 xs N mulEnabled acc
part2 ('\'':xs) N mulEnabled acc = part2 xs Quote mulEnabled acc
part2 ('t':xs) Quote mulEnabled acc = part2 xs T mulEnabled acc
part2 ('(':xs) L mulEnabled acc = part2 xs (LParen Mul) mulEnabled acc
part2 ('(':xs) O mulEnabled acc = part2 xs (LParen Do) mulEnabled acc
part2 ('(':xs) T mulEnabled acc = part2 xs (LParen Dont) mulEnabled acc
part2 (x:xs) (LParen Mul) mulEnabled acc
    | isDigit x = part2 xs (Num1 [x]) mulEnabled acc
    | otherwise = part2 xs Start mulEnabled acc
part2 (',':xs) (Num1 y) mulEnabled acc = part2 xs (Comma y) mulEnabled acc
part2 (x:xs) (Num1 y) mulEnabled acc
    | isDigit x && length y < 3 = part2 xs (Num1 (x:y)) mulEnabled acc
    | otherwise = part2 xs Start mulEnabled acc
part2 (x:xs) (Comma other) mulEnabled acc
    | isDigit x = part2 xs (Num2 [x] other) mulEnabled acc
    | otherwise = part2 xs Start mulEnabled acc
part2 (')':xs) (LParen Do) _ acc = part2 xs Start True acc
part2 (')':xs) (LParen Dont) _ acc = part2 xs Start False acc
part2 (')':xs) (Num2 a b) True acc = part2 xs Start True (acc + ((read $ reverse a) *  (read $ reverse b)))
part2 (')':xs) (Num2 a b) False acc = part2 xs Start False acc
part2 (x:xs) (Num2 y other) mulEnabled acc
    | isDigit x = part2 xs (Num2 (x:y) other) mulEnabled acc
    | otherwise = part2 xs Start mulEnabled acc
part2 (_:xs) _ mulEnabled acc = part2 xs Start mulEnabled acc
