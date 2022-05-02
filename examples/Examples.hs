{-# language PackageImports #-}
{-# language TemplateHaskell #-}

import Data.Typeable (Typeable)
import qualified Examples.Snarkl as Snarkl
import qualified Examples.Straw as Straw
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified "snarkl" Compile as Snarkl
import qualified "snarkl" Toplevel as Snarkl

-- | Little helper to get our expressions into a form that Hedgehog can compare.
comparable :: Typeable ty => Snarkl.Comp ty -> String
comparable = show . Snarkl.r1cs_of_comp Snarkl.Simplify

prop_p1 :: Hedgehog.Property
prop_p1 = Hedgehog.property $ comparable Snarkl.p1 === comparable Straw.p1

main :: IO ()
main =
  Hedgehog.defaultMain . pure $ Hedgehog.checkParallel $$(Hedgehog.discover)
