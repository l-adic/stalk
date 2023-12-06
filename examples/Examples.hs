{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Typeable (Typeable)
import qualified Examples.Arithmetic as Arithmetic
import qualified Examples.SnarklUnitTests as SnarklUnitTests
import qualified Examples.Snarkl as Snarkl
import qualified Examples.Straw as Straw
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Main as Arithmetic
import qualified Hedgehog.Main as Hedgehog
import qualified "snarkl" Compile as Snarkl
import qualified "snarkl" Toplevel as Snarkl
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as Hedgehog

-- | Little helper to get our expressions into a form that Hedgehog can compare.
comparable :: (Typeable ty) => Snarkl.Comp ty -> String
comparable = show . Snarkl.r1cs_of_comp Snarkl.Simplify

interpretable :: (Typeable ty) => Snarkl.Comp ty -> [Rational] -> Rational
interpretable = Snarkl.comp_interp

-- prop_p1 :: Hedgehog.Property
-- prop_p1 = Hedgehog.property $ comparable Snarkl.p1 === comparable Straw.p1

prop_simple_bool :: Hedgehog.Property
prop_simple_bool =
  Hedgehog.property $
    interpretable Arithmetic.simpleBool [2, 9] === 42

prop_simple_arith :: Hedgehog.Property
prop_simple_arith =
  Hedgehog.property $
    interpretable Arithmetic.simpleArith [2] === 17

-- prop_prog1 :: Hedgehog.Property
-- prop_prog1 = 
--   Hedgehog.property $
--     interpretable SnarklUnitTests.prog1 [1,2,1] === -240

prop_bool_prog6 :: Hedgehog.Property
prop_bool_prog6 = 
  Hedgehog.property $ do
    x <- Hedgehog.forAll Gen.bool_
    y <- Hedgehog.forAll Gen.bool_
    let x' = if x then 1 else 0
    let y' = if y then 1 else 0
    interpretable SnarklUnitTests.bool_prog9 [x',y'] === if (x && y) then 1 else 0   

main :: IO ()
main = do
  setLocaleEncoding utf8
  Hedgehog.defaultMain . pure $ Hedgehog.checkParallel $$(Hedgehog.discover)
