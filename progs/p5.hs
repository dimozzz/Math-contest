c       :: Integer -> Integer -> Integer
c 0 0   = 1
c 0 _   = 0
c _ 0   = 1
c n k   = (c (n - 1) (k - 1)) + (c (n - 1) k) 

sumc     :: Integer -> Integer
sumc n   = foldl (+) 0 [(c n x) | x <- [2..(n-1) `div` 2], x `mod` 3 == 2]

res     :: Integer -> Integer
res n   = power (n - 1) - (3 * sumc n) - 1

power   :: Integer -> Integer
power n = product(replicate (fromInteger n) 2) 

findPairs   :: Int -> [(Int, Int)]
findPairs p = [(a, b) | a <- [1..p-1], b <- [1..p-1], (a * a + a * b + b * b) `mod` p == 0]

pairExists :: Int -> Bool
pairExists = not . null . findPairs

primes :: [Int]
primes = filter isPrime [5..] where
            isPrime     :: Int -> Bool
            isPrime n   = not (any ((== 0) . (mod n)) [2..floor (sqrt (fromIntegral n))])

hyphotesis = all ((== 1) . (`mod` 6)) (filter pairExists primes)
