{-
  Extended complex numbers
  2009 Nathan Blythe, Dr. Oscar Boykin

  The extended complex numbers consist of all numbers of the form
  a + b * i, where a and b are extended rationals.
-}

module ExtCpx (ExtCpx (ExtCpx)) where

import ExtRat

{-
  Data type definition.  An extended complex number is stored as
  an ExtRat double.
-}
data ExtCpx = ExtCpx ExtRat ExtRat

{-
  Implementation of the Num class.
-}
instance Num ExtCpx where
  (ExtCpx a1 b1) + (ExtCpx a2 b2) = ExtCpx (a1 + a2) (b1 + b2)
  (ExtCpx a1 b1) * (ExtCpx a2 b2) = ExtCpx (a1 * a2 - b1 * b2) (a1 * b2 + a2 * b1)
  negate (ExtCpx a b) = ExtCpx (negate a) (negate b)
  abs (ExtCpx a b) = ExtCpx (a * a + b * b) 0
  signum (ExtCpx a b) = error "signum undefined for extended complex numbers"
  fromInteger n = ExtCpx (fromInteger n) 0

{-
  Implementation of the Eq class.
-}
instance Eq ExtCpx where
  (ExtCpx a1 b1) == (ExtCpx a2 b2) = (a1 == a2) && (b1 == b2)

{-
  Implementation of the Show class.
-}
instance Show ExtCpx where
  showsPrec d (ExtCpx a b) r = (showsPrec d a "") ++ " + i*" ++ (showsPrec d b "") ++ r

