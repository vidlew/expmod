--Changing p requires changing a global variable. Hopefully a future version of Haskell will have dependent types.

module P_Adic where

import Data.Ratio
import Expmod

--E.g., set p=2 for dyadic rationals, p=7 for heptadic rationals, etc.
--p should be prime
p :: Integer
p = 7

dig :: Int
dig = 50

--p-adic rationals
data PAdic = P_Adic Int [Integer]

instance Show PAdic where{
  show (P_Adic e xs) = "..." ++ if e>=0 || xs!!(-e-1)==0 then reverse $ take dig $ (take e $ cycle "0") ++ (drop (-e) w)
                                                         else reverse $ take (dig-e+1) $ (take (-e) w)++'.':(drop (-e) w)
                                                  where w   = (f $ xs!!(dig+(if e>=0 then 0 else -1-e)))++(cycle "0")
                                                        f 0 = ""
                                                        f n = (if n`mod`p<=9 then toEnum $ (fromEnum '0')+(fromIntegral $ n`mod`p) else toEnum $ (fromEnum 'a')+(fromIntegral $ (n-10)`mod`p)):(f $ n`div`p)
}

normalise :: PAdic -> PAdic
normalise (P_Adic e (0:xs)) = normalise $ P_Adic (e+1) $ (`div`p) <$> xs
normalise x = x

instance Num PAdic where{
  fromInteger 0                 = P_Adic 0 $ cycle [0]
--; fromInteger n                 = let e = head $ reverse $ takeWhile (\x -> n`mod`(p^x)==0) [0..] in P_Adic e $ ((n`div`(p^e))`mod`) <$> (p^) <$> [1..]
; fromInteger n                 = P_Adic 0 $ (n`mod`) <$> (p^) <$> [1..]
; (P_Adic e xs) + (P_Adic f ys) = P_Adic (min e f) $ zipWith (mod) (zipWith (+) ((*p^(max 0 $ e-f))<$>xs) ((*p^(max 0 $ f-e))<$>ys)) $ (p^) <$> [1..]
; (P_Adic e xs) * (P_Adic f ys) = P_Adic (e+f) $ zipWith (mod) (zipWith (*) xs ys) $ (p^) <$> [1..]
; negate (P_Adic e xs)          = P_Adic e $ zipWith (\x y -> ((p^y)-x)`mod`(p^y)) xs [1..]
; abs (P_Adic e (0:xs))         = abs $ P_Adic (e+1) xs
; abs (P_Adic e _)              = P_Adic (-e) $ cycle [1]
; signum x                      = x*(abs x)
}

--Determine whether the p-adic norm of x is at most p^-n
ordge :: PAdic -> Int -> Bool
ordge (P_Adic e (0:xs)) n = e>=n || (ordge (P_Adic e $ (`div`p) <$> xs) $ n-1)
ordge (P_Adic e _) n      = e>=n

--Limit of a, a^a, a^a^a, a^a^a^a, ...
tower :: Integer -> PAdic
tower a = P_Adic 0 $ (a`towermod`) <$> (p^) <$> [1..]

instance Fractional PAdic where{
  fromRational r      = (fromInteger $ numerator r)/(fromInteger $ denominator r)    --P_Adic $ (\x -> ((numerator r)*(invmod (denominator r) x))`mod`x) <$> l where l = p:((p*)<$>l)
; recip (P_Adic e xs) = (\(P_Adic e' xs') -> P_Adic (-e') $ zipWith invmod xs' $ l) $ normalise $ P_Adic e xs where l = p:((p*)<$>l)
}

instance Floating PAdic where{
  pi = error "π is not a p-adic number!"
}

some :: ([Integer] -> Bool) -> Bool
some f = f $ find f

all :: ([Integer] -> Bool) -> Bool
all = not.some.(not.)

find :: ([Integer] -> Bool) -> [Integer]
--find f = g [0..(p-1)] where g (x:xs) = if some (f.(x:)) then x:(find $ f.(x:)) else g xs
--                            g _      = 0:(find $ f.(0:))
find f = h:(find $ f.(h:)) where h = g [0..p-1]
                                 g (x:xs) = if f $ x:(find $ f.(x:)) then x else g xs
                                 g _ = 0

undigits :: Int -> [Integer] -> PAdic
undigits e (d:ds) = P_Adic e $ scanl (+) d $ zipWith (*) ds $ (p^) <$> [1..]
