{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
zero :: Integer
zero = 0

successor :: Integer -> Integer
successor = (+) 1

double :: Integer -> Integer
double x = x + x
-- double = (*) 2

twice :: (t -> t) -> t -> t
twice f = f . f

predecessor :: Integer -> Integer
predecessor 0 = 0
predecessor x = x - 1

add :: Integer -> Integer -> Integer
add 0 y = y
add x y = add (predecessor x) (successor y)

minus :: Integer -> Integer -> Integer
minus x 0 = x
minus x y = minus (predecessor x) (predecessor y)

times :: Integer -> Integer -> Integer
times 0 y = 0
times x y
    | x < 0 = times (-x) (-y)
    | otherwise = add (times (predecessor x) y) y

times' :: Integer -> Integer -> Integer
times' x 0 = 0
times' x y = repeat' (add x) y 0

repeat' :: (t -> t) -> Integer -> (t -> t)
repeat' f 1 = f
repeat' f n = f . repeat' f (n-1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)



fibonacci :: Integer -> Integer
-- Input : n      Output: nth fibonacci number
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibonacci' :: Integer -> Integer
fibonacci' n = helper 1 1 n
    where
        helper :: Integer -> Integer -> Integer -> Integer
        helper x x' 0 = x'
        helper x x' n = helper (x + x') x (n-1)

sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + sumList xs

sumList' :: [Integer] -> Integer
sumList' = helper 0
    where
        helper s [] = s
        helper s (x : xs) = helper (s + x) xs

maxNum :: Integer -> Integer -> Integer
-- maxNum a b = a if a > b else b
maxNum a b
    | a > b     = a
    | otherwise = b

maxList :: [Integer] -> Integer
maxList [] = 0
maxList [x] = x
maxList (x:xs) = maxNum x (maxList xs)

reverse' :: [Integer] -> [Integer]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [Integer] -> [Integer]
reverse'' [] = []
reverse'' xs = helper xs []
    where
        helper [] ys = ys
        helper (x:xs) ys = helper xs (x:ys)

exp' :: Integer -> Integer -> Integer
-- exp x y = x^y
exp' a 0 = 1
exp' a b = a * exp' a (b-1)

exp'' :: Integer -> Integer -> Integer
exp'' x =  (!!) (iterate (*x) x) . fromIntegral


length' :: [Integer] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

--doubleList [x,y,z,...] = [2x, 2y, 2z, ...]

map' :: (Integer -> Integer) -> [Integer] -> [Integer]
-- map f [x, y, ...] = [f x, f y, ...]
map' f [] = []
map' f (x:xs) = f x : xs

--productList [x,y,z,...] = x*y*z*...

foldl' :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
-- foldl f i [x,y,z] = f (f (f i x) y) z
-- sumList, productList の一般化
foldl' _  i [] = i
foldl' f i (x:xs) = foldl' f (f i x) xs

flatten :: [[Integer]] -> [Integer]
-- flatten [[2,3], [3,4], [1,2,3,4]] = [2,3,3,4,1,2,3,4]
flatten [] = []
flatten (xs : xss) = xs ++ flatten xss

flatten' :: [[Integer]] -> [Integer]
flatten' = foldr (++) [] -- =concat

mergeSort :: [Integer] -> [Integer]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge as bs
    where
        (as', bs') = split xs
        (as, bs) = (mergeSort as', mergeSort bs')
        merge :: [Integer] -> [Integer] -> [Integer]
        merge (x:xs) (y:ys)
            | x <= y = x : merge xs (y:ys)
            | otherwise = y : merge (x:xs) ys
        merge xs ys = xs ++ ys
        split :: [Integer] -> ([Integer], [Integer])
        split = splitAt =<< flip div 2 . (+1) . length
