module Golf where

-- exercise 1
-- 返回一个列表，每个元素应当为间隔及起始位置分别从 1 增加至最大长度-1的列表子集
-- skips [1..9]
-- 1, 2, 3, 4, 5, 6, 7, 8, 9
-- 2, 4, 6, 8
-- 3, 6, 9
-- 4, 8
-- 5
-- 6
-- 7
-- 8
-- 9

skips :: [a] -> [[a]]
skips lst = let lstLen = length lst in
  [subList i lst lstLen | i <- [1..lstLen]]
  where subList step lst len = [lst !! i | i <- [step-1, step*2-1 ..len-1]]


-- exercise 2
-- 返回列表中比周围元素都大的元素
localMaxima :: [Integer] -> [Integer]
localMaxima lst
  | length lst > 2 = foldl isMaxima [] [1..length lst-2]
  | otherwise = []
    where isMaxima l i=
            let left  = lst !! (i - 1)
                mid   = lst !! i
                right = lst !! (i + 1) in
                    if left < mid && mid > right then [mid] ++ l else l


-- exercise 3
-- 计算元素出现次数
count :: [Integer] -> [Int]
count lst = [length $ filter (== i) lst | i <- [0..9]]

-- 生成一行
genLine :: [Int] -> Int -> String
genLine lst n = [if i >= n then '*' else ' ' | i <- lst]

bottom = "==========\n0123456789\n"

{- [1, 1, 1, 5]
3   *
2   *
1   *   *
   ==========
   0123456789
元素的出现次数大于等于行号输出 '*' 否则 ' '
 - -}
histogram :: [Integer] -> String
histogram lst = let c = count lst
                    height = maximum c in
                    unlines (map (genLine c) [height, height-1..1]) ++ bottom
