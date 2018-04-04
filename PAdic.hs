module PAdic (mkPAdic, PAdic, ordge) where

import Data.Ratio
import Expmod
--import Search
import TypeDecimal

type Sequence = Int -> Integer

zipWithS :: (a -> b -> c) -> (Int -> a) -> (Int -> b) -> (Int -> c)
zipWithS f g h n = (g n) `f` (h n)

dig :: Int
dig = 50

--p-adic rationals
data PAdic p = P_Adic p Int Sequence

instance (Dec p) => Show (PAdic p) where{
  show (P_Adic u e s) = "..." ++ if e>=0 then reverse $ take dig $ (take e $ cycle "0") ++ (drop (-e) w)
                                         else reverse $ dropWhile (=='.') $ dropWhile (=='0') $ take (dig-e+1) $ (take (-e) w)++'.':(drop (-e) w)
                                 where p = decval u
                                       w   = (f $ s (dig+(if e>=0 then 0 else -e)))++(cycle "0")
                                       f 0 = ""
                                       f n = (if n`mod`p<=9 then toEnum $ (fromEnum '0')+(fromIntegral $ n`mod`p) else toEnum $ (fromEnum 'a')+(fromIntegral $ (n-10)`mod`p)):(f $ n`div`p)
}

normalise :: (Dec p) => (PAdic p) -> (PAdic p)
normalise (P_Adic u e s) = if s 0 == 0 then normalise $ P_Adic u (e+1) $ (`div`(decval u)) <$> (s.(+1)) else P_Adic u e s

base :: (Dec p) => (PAdic p) -> Integer
base (P_Adic u _ _) = decval u

mkPAdic :: (Dec p) => p -> Rational -> (PAdic p)
mkPAdic _ q = fromRational q

instance (Dec p) => Num (PAdic p) where{
  fromInteger 0                   = P_Adic undefined 0 $ const 0
; fromInteger n                   = normalise x where x = P_Adic undefined 0 $ \m -> n`mod`(p^(m+1))
                                                      p = base x
; (P_Adic u e s) + (P_Adic _ f t) = P_Adic u (min e f) $ zipWithS mod (zipWithS (+) ((*p^(max 0 $ e-f)).s) ((*p^(max 0 $ f-e)).t)) $ ((p^).(+1)) where p = decval u
; (P_Adic u e s) * (P_Adic _ f t) = P_Adic u (e+f) $ zipWithS mod (zipWithS (*) s t) $ ((p^).(+1)) where p = decval u
; negate (P_Adic u e s)           = P_Adic u e $ \m -> (-s m)`mod`(p^(m+1)) where p = decval u
; abs (P_Adic u e s)              = if s 0 == 0 then abs $ P_Adic u (e+1) (s.(+1)) else P_Adic u (-e) $ const 1
; signum x                        = x*(abs x)
}

--Determine whether the p-adic norm of x is at most p^-n
ordge :: (Dec p) => (PAdic p) -> Int -> Bool
ordge (P_Adic u e s) n = e>=n || ((s 0==0) && (ordge (P_Adic u e $ (`div`p).s.(+1)) $ n-1)) where p = decval u


--Limit of a, a^a, a^a^a, a^a^a^a, ...
tower :: (Dec p) => p -> Integer -> (PAdic p)
tower u a = P_Adic u 0 $ (a`towermod`).((decval u)^).(+1)


factorial :: Int -> Integer
factorial n = foldl (*) 1 $ take n [1..]


--Largest power of p by which n! is divisible, assuming p is prime
ordfact :: Integer -> Int -> Int
ordfact _ 0 = 0
ordfact p n = (n`div`(fromInteger p))+(ordfact p $ n`div`(fromInteger p))


instance (Dec p) => Fractional (PAdic p) where{
  fromRational r      = (fromInteger $ numerator r)/(fromInteger $ denominator r)
; recip (P_Adic u e s) = (\(P_Adic _ e' s') -> P_Adic u (-e') $ zipWithS invmod s' $ l) $ normalise $ P_Adic u e s where l = \n -> p^(n+1)
                                                                                                                         p = decval u
}

{-
instance (Dec p) => Floating (PAdic p) where{
  pi = error "π is not a p-adic number!"
; exp x@(P_Adic u e s) = if x `ordge` 1 then P_Adic u e $ \n -> s n else error "Divergent series"
--; log
--; sin
--; cos
--; asin
--; acos
--; atan
--; sinh
--; cosh
--; asinh
--; acosh
--; atanh
--; sqrt
}

-}
