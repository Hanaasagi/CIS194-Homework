-- exercise 1
rev :: [t] -> [t]
rev [] = []
rev (a: l) = rev l ++ [a]


toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []


toDigits :: Integer -> [Integer]
toDigits n = rev $ toDigitsRev n


-- exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = zipWith (*) oneTwo l
  where oneTwo = if odd $ length l then 1 : 2 : oneTwo else 2 : 1 : oneTwo


-- exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)


-- exercise 4
validate :: Integer -> Bool
validate n
  | n > 0     = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0
  | otherwise = False
