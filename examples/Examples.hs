{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Fin (fin0, fin4, fin5)
import Data.Typeable (Typeable)
import qualified Examples.Arithmetic as Arithmetic
import qualified Examples.Lens as Lens
import qualified Examples.SnarklUnitTests as SnarklUnitTests
import Examples.Sudoku
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified Snarkl.Compile as Snarkl
import qualified Snarkl.Toplevel as Snarkl

-- | Little helper to get our expressions into a form that Hedgehog can compare.
comparable :: (Typeable ty) => Snarkl.Comp ty -> String
comparable = show . Snarkl.r1cs_of_comp Snarkl.Simplify

interpretable :: (Typeable ty) => Snarkl.Comp ty -> [Rational] -> Rational
interpretable = Snarkl.comp_interp

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

prop_validate_sudoku :: Hedgehog.Property
prop_validate_sudoku =
  Hedgehog.property $
    validateSudoku (sudokuFromList validPuzzule) === True

prop_invalidate_sudoku :: Hedgehog.Property
prop_invalidate_sudoku =
  Hedgehog.property $
    interpretable sudoku (concat invalidSudokuPuzzle) === 0

main :: IO ()
main = do
  setLocaleEncoding utf8
  Hedgehog.defaultMain . pure $ Hedgehog.checkParallel $$(Hedgehog.discover)
