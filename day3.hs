import Data.Char (isDigit)

main = do
    input <- getContents
    print $ part1 input
    print $ part2 input

data Method = Mul | Do | Dont
data State = Start | M | U | L | LParen Method | Num1 String | Comma String | Num2 String String | D | O | N | Quote | T

part1 :: String -> Int
part1 input = step (filter (/= 'd') input) Start True 0

part2 :: String -> Int
part2 input = step input Start True 0

step :: String -> State -> Bool -> Int -> Int
step "" _ _ acc = acc
step ('m':xs) Start mulEnabled acc = step xs M mulEnabled acc
step ('u':xs) M mulEnabled acc = step xs U mulEnabled acc
step ('l':xs) U mulEnabled acc = step xs L mulEnabled acc
step ('d':xs) Start mulEnabled acc = step xs D mulEnabled acc
step ('o':xs) D mulEnabled acc = step xs O mulEnabled acc
step ('n':xs) O mulEnabled acc = step xs N mulEnabled acc
step ('\'':xs) N mulEnabled acc = step xs Quote mulEnabled acc
step ('t':xs) Quote mulEnabled acc = step xs T mulEnabled acc
step ('(':xs) L mulEnabled acc = step xs (LParen Mul) mulEnabled acc
step ('(':xs) O mulEnabled acc = step xs (LParen Do) mulEnabled acc
step ('(':xs) T mulEnabled acc = step xs (LParen Dont) mulEnabled acc
step (x:xs) (LParen Mul) mulEnabled acc
    | isDigit x = step xs (Num1 [x]) mulEnabled acc
    | otherwise = step xs Start mulEnabled acc
step (',':xs) (Num1 y) mulEnabled acc = step xs (Comma y) mulEnabled acc
step (x:xs) (Num1 y) mulEnabled acc
    | isDigit x && length y < 3 = step xs (Num1 (x:y)) mulEnabled acc
    | otherwise = step xs Start mulEnabled acc
step (x:xs) (Comma other) mulEnabled acc
    | isDigit x = step xs (Num2 [x] other) mulEnabled acc
    | otherwise = step xs Start mulEnabled acc
step (')':xs) (LParen Do) _ acc = step xs Start True acc
step (')':xs) (LParen Dont) _ acc = step xs Start False acc
step (')':xs) (Num2 a b) True acc = step xs Start True (acc + ((read $ reverse a) *  (read $ reverse b)))
step (')':xs) (Num2 a b) False acc = step xs Start False acc
step (x:xs) (Num2 y other) mulEnabled acc
    | isDigit x = step xs (Num2 (x:y) other) mulEnabled acc
    | otherwise = step xs Start mulEnabled acc
step (_:xs) _ mulEnabled acc = step xs Start mulEnabled acc
