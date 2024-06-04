buildingMain :: IO ()
buildingMain = do
    getLine
    b :: [Int] <- map read . words <$> getLine
    print $ head $ dropWhile (==0) [building x b | x <- [1..length b-1]]++[-1]

building :: Int -> [Int] -> Int
building x y =
    if head y < (y !! x) then x+1 else 0

exp' :: Integer -> Integer -> Integer
exp' x 0 = 1
exp' x y = (*) x (exp' x (y-1))

exp'' :: Integer -> Integer -> Integer
exp'' x =  (!!) (iterate (*x) x) . fromIntegral

length' :: [Integer] -> Integer
length' [] = 0
length' (x : xs) = length' xs + 1

map' :: (Integer -> Integer) -> [Integer] -> [Integer]
-- map f [x, y, ...] = [f x, f y, ...]
map' f [] = []
map' f (x:xs) = f x : map' f xs

foldl' :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldl' f i [] = i
foldl' f i (x:xs) = f (foldl' f i xs) x

foldr' :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldr' f i [] = 0
foldr' f i (x : xs) = foldr' f (f i x) xs

flatten' :: [[Integer]] -> [Integer]
flatten' [] = []
flatten' (x:xs) = x ++ flatten' xs

-- mergeSort :: [Integer] -> [Integer]
-- mergeSort [] = []
-- mergeSort [x] = [x]
-- mergeSort xs = merge as bs
--     where
--         (as', bs') = split xs
--         (as, bs) = (mergeSort as', mergeSort bs')
--         merge :: [Integer] -> [Integer] -> [Integer]
--         merge (x:xs) (y:ys)
--             | x <= y = x : merge xs (y:ys)
--             | otherwise = y : merge (x:xs) ys
--         merge xs ys = xs ++ ys
--         split :: [Integer] -> ([Integer], [Integer])
--         split = splitAt =<< flip div 2 . (+1) . length

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort s = merge (mergeSort (take t s)) (mergeSort (drop t s))
    where
        t = length s `div` 2