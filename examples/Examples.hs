{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Field.Galois (GaloisField)
import Data.Typeable (Typeable)
import qualified Examples.Arithmetic as Arithmetic
import qualified Examples.Lens as Lens
import qualified Examples.SnarklUnitTests as SnarklUnitTests
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified Snarkl.Compile as Snarkl
import qualified Snarkl.Toplevel as Snarkl

-- | Little helper to get our expressions into a form that Hedgehog can compare.
comparable :: (Typeable ty, GaloisField k) => Snarkl.Comp ty k -> String
comparable = show . Snarkl.compileCompToR1CS [Snarkl.Simplify]

interpretable :: (Typeable ty, GaloisField k) => Snarkl.Comp ty k -> [k] -> k
interpretable comp is = Snarkl.comp_interp comp is mempty

prop_simple_bool :: Hedgehog.Property
prop_simple_bool =
  Hedgehog.property $
    interpretable Arithmetic.simpleBool [2, 9] === 42

prop_simple_arith :: Hedgehog.Property
prop_simple_arith =
  Hedgehog.property $
    interpretable Arithmetic.simpleArith [2] === 17

prop_prog1 :: Hedgehog.Property
prop_prog1 =
  Hedgehog.property $
    interpretable SnarklUnitTests.prog1 [1, 2, 1] === -240

prop_simple_lens :: Hedgehog.Property
prop_simple_lens =
  Hedgehog.property $
    interpretable Lens.simpleLens [1, 2] === 42

-- prop_complicated_lens :: Hedgehog.Property
-- prop_complicated_lens =
--  Hedgehog.property $
--    interpretable Lens.simpleLens [1, 2, 32] === 42

main :: IO ()
main = do
  setLocaleEncoding utf8
  Hedgehog.defaultMain . pure $ Hedgehog.checkParallel $$(Hedgehog.discover)
