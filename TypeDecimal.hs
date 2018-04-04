{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

--Type-level decimal arithmetic
module TypeDecimal (zero, one, two, three, four, five, six, seven, eight, nine, (&), Dec, decval, decimal, decimate, times, plus) where

data Nil

data Zero a
data One a
data Two a
data Three a
data Four a
data Five a
data Six a
data Seven a
data Eight a
data Nine a

class Digit b where{
  shift :: (b a) -> a
; digval :: (b a) -> Integer
}

instance Digit Zero where{
  shift = \_ -> undefined
; digval = \_ -> 0 
}

instance Digit One where{
  shift = \_ -> undefined
; digval = \_ -> 1
}

instance Digit Two where{
  shift = \_ -> undefined
; digval = \_ ->2
}

instance Digit Three where{
  shift = \_ -> undefined
; digval = \_ -> 3
}

instance Digit Four where{
  shift = \_ -> undefined
; digval = \_ -> 4
}

instance Digit Five where{
  shift = \_ -> undefined
; digval = \_ -> 5
}

instance Digit Six where{
  shift = \_ -> undefined
; digval = \_ -> 6
}

instance Digit Seven where{
  shift = \_ -> undefined
; digval = \_ -> 7
}

instance Digit Eight where{
  shift = \_ -> undefined
; digval = \_ -> 8
}

instance Digit Nine where{
  shift = \_ -> undefined
; digval = \_ -> 9
}

class Dec a where{
  decval :: a -> Integer
}

instance Dec Nil where
  decval = \_ -> 0

instance (Dec a, Digit b) => Dec (b a) where{
  decval x = (digval x) +10*(decval $ shift x)
}

infixl 1 &
(&) :: (a -> b) -> (b -> c) -> a -> c
f&g = g.f

zero :: a -> Zero a
zero = \_ -> undefined

one :: a -> One a
one = \_ -> undefined

two :: a -> Two a
two = \_ -> undefined

three :: a -> Three a
three = \_ -> undefined

four :: a -> Four a
four = \_ -> undefined

five :: a -> Five a
five = \_ -> undefined

six :: a -> Six a
six = \_ -> undefined

seven :: a -> Seven a
seven = \_ -> undefined

eight :: a -> Eight a
eight = \_ -> undefined

nine :: a -> Nine a
nine = \_ -> undefined

decimal :: (Nil -> a) -> a
decimal = ($ undefined)

data Decimal = forall a. Dec a => Decimal a

instance Dec Decimal where
  decval (Decimal x) = decval x

decimate :: Integer -> Decimal
decimate 0 = Decimal (undefined :: Nil)
decimate n = case n`mod`10 of 0 -> (\(Decimal x) -> Decimal $ zero  x) $ decimate $ n`div`10
                              1 -> (\(Decimal x) -> Decimal $ one   x) $ decimate $ n`div`10
                              2 -> (\(Decimal x) -> Decimal $ two   x) $ decimate $ n`div`10
                              3 -> (\(Decimal x) -> Decimal $ three x) $ decimate $ n`div`10
                              4 -> (\(Decimal x) -> Decimal $ four  x) $ decimate $ n`div`10
                              5 -> (\(Decimal x) -> Decimal $ five  x) $ decimate $ n`div`10
                              6 -> (\(Decimal x) -> Decimal $ six   x) $ decimate $ n`div`10
                              7 -> (\(Decimal x) -> Decimal $ seven x) $ decimate $ n`div`10
                              8 -> (\(Decimal x) -> Decimal $ eight x) $ decimate $ n`div`10
                              9 -> (\(Decimal x) -> Decimal $ nine  x) $ decimate $ n`div`10

{-
data Z
data S a
class Peano a where toInt :: a -> Int
instance Peano Z where toInt = \_ -> 0
decrement :: (S a) -> a
decrement = \_ -> undefined
instance (Peano a) => Peano (S a) where toInt x = 1+(toInt $ decrement x)
class Plus a b c | a b -> c where plus :: a -> b -> c
instance Plus a Z a where plus = \_ _ -> undefined
instance (Plus a b c) => Plus a (S b) (S c) where plus = \_ _ -> undefined
class Times a b c | a b -> c where times :: a -> b -> c
instance Times Z b Z where times = \_ _ -> undefined
instance (Times a b c, Plus b c d) => Times (S a) b d where times = \_ _ -> undefined
class Nat a b | a -> b where toPeano :: a -> b
instance Nat Nil Z where toPeano = \_ -> undefined
type Ten = S (S (S (S (S (S (S (S (S (S Z)))))))))
instance (Nat a b, Times Ten b c) => Nat (Zero a) c where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S Z) c d) => Nat (One a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S Z)) c d) => Nat (Two a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S Z))) c d) => Nat (Three a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S (S Z)))) c d) => Nat (Four a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S (S (S Z))))) c d) => Nat (Five a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S (S (S (S Z)))))) c d) => Nat (Six a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S (S (S (S (S Z))))))) c d) => Nat (Seven a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S (S (S (S (S (S Z)))))))) c d) => Nat (Eight a) d where toPeano   = \_ -> undefined
instance (Nat a b, Times Ten b c, Plus (S (S (S (S (S (S (S (S (S Z))))))))) c d) => Nat (Nine a) d where toPeano   = \_ -> undefined
-}


class Plus a b c | a b -> c where plus :: a -> b -> c

instance Plus Nil b b where plus = \_ _ -> undefined
instance Plus (Zero a) Nil (Zero a) where plus = \_ _ -> undefined
instance Plus (One a) Nil (One a) where plus = \_ _ -> undefined
instance Plus (Two a) Nil (Two a) where plus = \_ _ -> undefined
instance Plus (Three a) Nil (Three a) where plus = \_ _ -> undefined
instance Plus (Four a) Nil (Four a) where plus = \_ _ -> undefined
instance Plus (Five a) Nil (Five a) where plus = \_ _ -> undefined
instance Plus (Six a) Nil (Six a) where plus = \_ _ -> undefined
instance Plus (Seven a) Nil (Seven a) where plus = \_ _ -> undefined
instance Plus (Eight a) Nil (Eight a) where plus = \_ _ -> undefined
instance Plus (Nine a) Nil (Nine a) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Zero a) (Zero b) (Zero c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (One b) (One c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Two b) (Two c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Three b) (Three c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Four b) (Four c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Five b) (Five c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Six b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Seven b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Eight b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Zero a) (Nine b) (Nine c) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (One a) (Zero b) (One c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (One b) (Two c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Two b) (Three c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Three b) (Four c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Four b) (Five c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Five b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Six b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Seven b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (One a) (Eight b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (One a) (Nine b) (Zero d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Two a) (Zero b) (Two c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (One b) (Three c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (Two b) (Four c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (Three b) (Five c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (Four b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (Five b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (Six b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Two a) (Seven b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Two a) (Eight b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Two a) (Nine b) (One d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Three a) (Zero b) (Three c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Three a) (One b) (Four c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Three a) (Two b) (Five c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Three a) (Three b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Three a) (Four b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Three a) (Five b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Three a) (Six b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Three a) (Seven b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Three a) (Eight b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Three a) (Nine b) (Two d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Four a) (Zero b) (Four c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Four a) (One b) (Five c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Four a) (Two b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Four a) (Three b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Four a) (Four b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Four a) (Five b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Four a) (Six b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Four a) (Seven b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Four a) (Eight b) (Two d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Four a) (Nine b) (Three d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Five a) (Zero b) (Five c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Five a) (One b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Five a) (Two b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Five a) (Three b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Five a) (Four b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Five a) (Five b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Five a) (Six b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Five a) (Seven b) (Two d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Five a) (Eight b) (Three d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Five a) (Nine b) (Four d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Six a) (Zero b) (Six c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Six a) (One b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Six a) (Two b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Six a) (Three b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Six a) (Four b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Six a) (Five b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Six a) (Six b) (Two d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Six a) (Seven b) (Three d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Six a) (Eight b) (Four d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Six a) (Nine b) (Five d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Seven a) (Zero b) (Seven c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Seven a) (One b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Seven a) (Two b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Three b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Four b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Five b) (Two d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Six b) (Three d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Seven b) (Four d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Eight b) (Five d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Seven a) (Nine b) (Six d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Eight a) (Zero b) (Eight c) where plus = \_ _ -> undefined
instance (Plus a b c) => Plus (Eight a) (One b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Two b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Three b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Four b) (Two d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Five b) (Three d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Six b) (Four d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Seven b) (Five d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Eight b) (Six d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Eight a) (Nine b) (Seven d) where plus = \_ _ -> undefined

instance (Plus a b c) => Plus (Nine a) (Zero b) (Nine c) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (One b) (Zero d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Two b) (One d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Three b) (Two d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Four b) (Three d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Five b) (Four d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Six b) (Five d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Seven b) (Six d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Eight b) (Seven d) where plus = \_ _ -> undefined
instance (Plus a b c, Plus (One Nil) c d) => Plus (Nine a) (Nine b) (Eight d) where plus = \_ _ -> undefined

testPlus :: (Dec a, Dec b, Dec c, Plus a b c) => a -> b -> Bool
testPlus x y = (decval x)+(decval y) == (decval $ x`plus`y)

class Times a b c | a b -> c where times :: a -> b -> c

instance Times Nil b Nil where times = \_ _ -> undefined
--instance (Digit d) => Times Nil (d b) Nil where times = \_ _ -> undefined

instance (Times a b c) => Times (Zero a) b (Zero c) where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d) => Times (One a) b d where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e) => Times (Two a) b e where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f) => Times (Three a) b f where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f, Plus b f g) => Times (Four a) b g where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f, Plus b f g, Plus b g h) => Times (Five a) b h where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f, Plus b f g, Plus b g h, Plus b h i) => Times (Six a) b i where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f, Plus b f g, Plus b g h, Plus b h i, Plus b i j) => Times (Seven a) b j where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f, Plus b f g, Plus b g h, Plus b h i, Plus b i j, Plus b j k) => Times (Eight a) b k where times = \_ _ -> undefined
instance (Times a b c, Plus b (Zero c) d, Plus b d e, Plus b e f, Plus b f g, Plus b g h, Plus b h i, Plus b i j, Plus b j k, Plus b k l) => Times (Nine a) b l where times = \_ _ -> undefined

testTimes :: (Dec a, Dec b, Dec c, Times a b c) => a -> b -> Bool
testTimes x y = (decval x)*(decval y) == (decval $ x`times`y)

{-
class ToThe a b c | a b -> c where toThe :: a -> b -> c

instance ToThe a Nil (One Nil) where toThe = \_ _ -> undefined
-}
