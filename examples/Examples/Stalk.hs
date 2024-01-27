{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Examples.Stalk
  ( mult_ex,
    arr_ex,
    p1,
    p2,
    comp1,
    comp2,
    -- test1,
  )
where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Vec.Client ()
import Data.Bool (bool)
import Data.Either (fromLeft)
import Data.Field.Galois (GaloisField)
import Data.Vec.Lazy (Vec (..))
import Snarkl.Field (F_BN128)
import qualified Snarkl.Language.Prelude as Snarkl
import Stalk
import "snarkl" Snarkl.Language.Prelude (Comp, fromField, (>>=))
import Prelude (Bool (..), Either (..), fromInteger, sum, ($), (*), (+), (.))

-- * Basic

mult_ex :: (GaloisField k) => k -> k -> k
mult_ex x y = x * y

-- | Compiling to Categories doesn't (yet) support direct indexing. But in this
--   case we can use a simple fold, which is much more common/recommended in
--   functional programs anyway.
arr_ex :: F_BN128 -> F_BN128
arr_ex x =
  let a = x ::: x ::: VNil
   in -- in (a ! 0) + (a ! 1)
      sum a

p1 :: Snarkl.Comp 'Snarkl.TField F_BN128
p1 = runStalk (Categorify.expression arr_ex) (fromField @F_BN128 1)

p2 :: Snarkl.Comp 'Snarkl.TField F_BN128
p2 =
  Snarkl.fresh_public_input
    >>= runStalk (Categorify.expression $ \(x :: F_BN128) -> x + x)

comp1 :: Either Bool F_BN128
comp1 = Left False

comp2 :: Either Bool F_BN128
comp2 = Right 0

-- | __TODO__: Needs @`ConCat.Category.ClosedCat` `Stalk`@
test1 :: Comp 'Snarkl.TBool F_BN128
test1 =
  Snarkl.fresh_public_input
    >>= runStalk
      (Categorify.expression $ fromLeft False . bool comp2 comp1)
