module FreeBool (fromBool, collapse, eliminate) where

import FreeBool.Types
import Data.Void (Void, absurd)

fromBool :: Bool -> FreeBool a
fromBool True  = BTrue
fromBool False = BFalse

eliminate :: (a -> Bool) -> FreeBool a -> FreeBool Void
eliminate f fa = fa >>= (fromBool . f)

collapse :: BoolAlg b => FreeBool Void -> b
collapse (BAnd lhs rhs) = collapse lhs &&& collapse rhs
collapse (BOr lhs rhs) = collapse lhs ||| collapse rhs
collapse (BNot e) = bnot (collapse e)
collapse BFalse = bfalse
collapse BTrue = btrue
collapse (Pure void) = absurd void
