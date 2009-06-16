{-
  Extended rationals
  2009 Nathan Blythe, Dr. Oscar Boykin

  The extended rationals consist of all numbers of the form
  a + b * sqrt(3)/2, where a and b are rationals.
-}

module ExtRat (ExtRat (ExtRat)) where

{-
  Data type definition.  An extended rational is stored as
  a Rational double.
-}
data ExtRat = ExtRat Rational Rational

{-
  Implementation of the Num class.
-}
instance Num ExtRat where
  (ExtRat a1 b1) + (ExtRat a2 b2) = ExtRat (a1 + a2) (b1 + b2)
  (ExtRat a1 b1) * (ExtRat a2 b2) = ExtRat (a1 * a2 + b1 * b2 * 3 / 4) (a1 * b2 + a2 * b1)
  negate (ExtRat a b) = ExtRat (negate a) (negate b)
  abs (ExtRat a b) = error "abs undefined for extended rationals"
  signum (ExtRat a b) = error "signum undefined for extended rationals"
  fromInteger n = ExtRat (fromInteger n) 0

{-
  Implementation of the Eq class.
-}
instance Eq ExtRat where
  (ExtRat a1 b1) == (ExtRat a2 b2) = (a1 == a2) && (b1 == b2)

{-
  Implementation of the Show class.
-}
instance Show ExtRat where
  showsPrec d (ExtRat a b) r = "(" ++ (showsPrec d a "") ++ ", " ++ (showsPrec d b "") ++ ")" ++ r

