{-
  Extended rationals
  2009 Nathan Blythe, Dr. Oscar Boykin

  The extended rationals consist of all numbers of the form
  a + b * sqrt(2) + c * sqrt(3) + d * sqrt(6), where a, b, c,
  and d are rationals.
-}

module ExtRat (ExtRat (ExtRat)) where

{-
  Data type definition.  An extended rational is stored as
  a Rational 4-tuple.
-}
data ExtRat = ExtRat Rational Rational Rational Rational

{-
  Implementation of the Num class.
-}
instance Num ExtRat where
  (ExtRat a1 b1 c1 d1) + (ExtRat a2 b2 c2 d2) = ExtRat (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)
  (ExtRat a1 b1 c1 d1) * (ExtRat a2 b2 c2 d2) = ExtRat (a1 * a2 + 2 * b1 * b2 + 3 * c1 * c2 + 6 * d1 * d2)
                                                       (a1 * b2 + b1 * a2 + 3 * c1 * d2 + 3 * d1 * c2)
                                                       (a1 * c2 + 2 * b1 * d2 + c1 * a2 + 2 * d1 * b2)
                                                       (a1 * d2 + b1 * c2 + c1 * b2 + d1 * a2)
  negate (ExtRat a b c d) = ExtRat (negate a) (negate b) (negate c) (negate d)
  abs (ExtRat a b c d) = error "abs undefined for extended rationals"
  signum (ExtRat a b c d) = error "signum undefined for extended rationals"
  fromInteger n = ExtRat (fromInteger n) 0 0 0

{-
  Implementation of the Eq class.
-}
instance Eq ExtRat where
  (ExtRat a1 b1 c1 d1) == (ExtRat a2 b2 c2 d2) = (a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2)

{-
  Implementation of the Show class.
-}
instance Show ExtRat where
  showsPrec v (ExtRat a b c d) r = "(" ++ (showsPrec v a "") ++ ", "
                                       ++ (showsPrec v b "") ++ ", "
                                       ++ (showsPrec v c "") ++ ", "
                                       ++ (showsPrec v d "") 
                                       ++ ")" ++ r

