{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
import Prelude hiding (div, mod, map, minimum)
zero :: Integer
zero = 0


one :: Integer
one = 1

helloWorld :: String
helloWorld = "Hello World!"

plus :: (Integer -> (Integer -> (Integer)))
plus x y = x + y

times :: Integer -> Integer -> Integer
-- times x y = x * y
times x 0 = 0
times x y = x + (((times) x) (y-1))

div :: Integer -> Integer -> Integer
div x y
    | y > x     = 0
    | otherwise = 1 + div (x - y) y

mod :: Integer -> Integer -> Integer
mod a n = a - times n (div a n)

isDivisible :: Integer -> Integer -> Bool
isDivisible a b = mod a b == 0

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = isPrime' n 2
    where
        isPrime' :: Integer -> Integer -> Bool
        isPrime' n a
            | a == n = True
            | a < n = not (isDivisible n a) && (isPrime' n (a+1))


primeFactors :: Integer -> [Integer]
primeFactors p = primeFactors' p 2 []
    where
        primeFactors' :: Integer -> Integer -> [Integer] -> [Integer]
        primeFactors' p n factors
            | n > p = factors
            | isPrime n && isDivisible p n = primeFactors' (div p n) n (n : factors)
            | isPrime n && not (isDivisible p n) = primeFactors' p (n+1) factors
            | otherwise = primeFactors' p (n+1) factors

isPrimeCool :: Integer -> Bool
isPrimeCool p = all ((/=0) . mod p) [2..p-1]

aaa :: Integer -> Bool
aaa n = minimum (map (mod n) [2 .. n-1]) == Just 0

map :: (Integer -> Integer) -> [Integer] -> [Integer]
map f [] = []
map f (x:xs) = f x : map f xs

minimum :: [Integer] -> Maybe Integer
minimum [] = Nothing
minimum (x:xs) = minimumWith xs x
    where
        minimumWith :: [Integer] -> Integer -> Maybe Integer
        minimumWith [] m = Just m
        minimumWith (x:xs) m
            | x < m = minimumWith xs x
            | otherwise = minimumWith xs m

fivetimesminimum :: [Integer] -> Maybe Integer
fivetimesminimum = fmap (*5) . minimum