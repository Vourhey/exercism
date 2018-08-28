module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Number a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (a, b) = Number a b

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Number a b) = Number a (-b)

abs :: Floating a => Complex a -> a
abs (Number a b) = sqrt $ a^2 + b^2

real :: Num a => Complex a -> a
real (Number a b) = a

imaginary :: Num a => Complex a -> a
imaginary (Number a b) = b

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Number a b) (Number c d) = Number (a * c - b * d) (b * c + a * d)

add :: Num a => Complex a -> Complex a -> Complex a
add (Number a b) (Number c d) = Number (a + c) (b + d)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Number a b) (Number c d) = Number (a - c) (b - d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Number a b) (Number c d) = Number r i
    where r = (a * c + b * d)/(c^2 + d^2)
          i = (b * c - a * d)/(c^2 + d^2)
