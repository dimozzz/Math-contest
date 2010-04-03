c       :: Integer -> Integer -> Integer
c 0 0   = 1
c 0 _   = 0
c _ 0   = 1
c n k   = (c (n - 1) (k - 1)) + (c (n - 1) k) 

sumc     :: Integer -> Integer
sumc n   = foldl (+) 0 [(c n x) | x <- [2..n-1], x `mod` 3 == 0]

res    :: Integer -> Integer
res n    = power n - sumc n - 2

power   :: Integer -> Integer
power n  = product(replicate (fromInteger n) 2) 
