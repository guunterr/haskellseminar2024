fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

main :: IO()
main = do print (fact 8)