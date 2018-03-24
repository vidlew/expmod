module Expmod where

data Bit = Zero | One deriving (Enum,Show,Eq)
type Natural = Integer

binary :: Natural -> [Bit]
binary 0 = []
binary x = (if x `mod` 2 == 0 then Zero else One):(binary $ x `div` 2)

--x^y (mod m)
expmod :: Natural -> Natural -> Natural -> Natural
expmod x y m = let s = iterate (\a -> a*a `mod` m) x
                   b = binary y in
               foldl (\x -> \y -> (x*y) `mod` m) 1 $ zipWith (\x -> \y -> if y == One then x else 1) s b

--Floor of base 2 logarithm
floorLg :: (Integral a, Integral b) => a -> b
floorLg 1 = 0
floorLg n = 1 + (floorLg $ n `div` 2)

--Tests whether or not a number is a perfect power
--Uses binary search, but there's probably a faster way using Newton
isPower :: Natural -> Bool
isPower 0 = True
isPower 1 = True
isPower n = let ps      = takeWhile (<= floorLg n) primes
                f a b p m = if (a^p==m) || (b^p==m) then True else if (abs $ a-b)<=1 then False else let c=((a+b) `div` 2) in if c^p<=m then f c b p m else f a c p m
            in foldr (||) False $ (\p -> f 1 n p n) <$> ps

primes :: [Natural]
primes = 2:3:(filter isPrime $ [6,12..] >>= \x -> [x-1,x+1])

--Agrawal-Kayal-Saxena test for primality
aks :: Natural -> Bool
aks 0 = False
aks 1 = False

isPrime :: Natural -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = foldr (&&) True $ ((/=0).(n `mod`)) <$> takeWhile (<= (floor $ sqrt $ fromIntegral n)) primes

π :: Natural -> Int
π n = length $ takeWhile (<=n) primes

factor :: Natural -> [(Natural,Natural)]
factor 1 = []
factor 0 = (\p -> let i=1+i in (p,i)) <$> primes
factor n = if isPrime n then [(n,1)] else (p,r):(factor $ n `div` (p^r)) where p = head $ filter ((==0).(n `mod`)) primes
                                                                               --r = head $ filter ((/=0).(n `mod`).(p^).(1+)) [0..]
                                                                               r = let f x = if x `mod` p == 0 then 1+(f $ x `div` p) else 0 in f n

φ :: Natural -> Natural
φ = (foldl (*) 1).((\(p,r) -> (p-1)*(p^(r-1)))<$>).factor

--Infinite tower of a's mod m (a^a^a^a^a^a... (mod m))
towermod :: Natural -> Natural -> Natural
towermod _ 0 = 0
towermod _ 1 = 0
towermod 1 _ = 1
towermod a m = if gcd a m == 1 then expmod a (towermod a $ φ m) m else ((towermod a g)*((m `div` g) `invmod` g)*(m `div` g)) `mod` m
                                                                         where g = let f x = if gcd a x == 1 then x else f $ x `div` (gcd a x) in f m

--Compute the inverse of a mod m, assuming a and m are coprime
invmod :: Natural -> Natural -> Natural
a `invmod` m = (a `expmod` ((φ m)-1)) m

