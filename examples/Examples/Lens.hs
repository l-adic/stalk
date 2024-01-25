{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Examples.Lens where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Client
import Control.Lens
import Snarkl.Field (F_BN128)
import Snarkl.Language (Ty (TField), pair, (>>=))
import qualified Snarkl.Language.SyntaxMonad as Snarkl
import Stalk
import Prelude (fromInteger)

data Point = Point {_x :: F_BN128, _y :: F_BN128}

type instance SnarklTy Point = SnarklTy (Rep Point)

$(deriveHasRep ''Point)
$(makeLenses ''Point)

data Atom = Atom {_elem :: F_BN128, _point :: Point, _foo :: F_BN128}

type instance SnarklTy Atom = SnarklTy (Rep Atom)

$(deriveHasRep ''Atom)
$(makeLenses ''Atom)

simpleLens :: Snarkl.Comp 'TField F_BN128
simpleLens =
  let prog :: (F_BN128, F_BN128) -> F_BN128
      prog (x, y) =
        let p = Point x y
            atom = Atom 32 p 1
            f :: Atom -> F_BN128 -> Atom
            f a n = a & elem +~ n
         in f atom 10 ^. elem

      compiledProg :: Stalk F_BN128 (F_BN128, F_BN128) F_BN128
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        p <- pair x y
        runStalk compiledProg p

-- complicatedLens :: Snarkl.Comp 'TField F_BN128
-- complicatedLens =
--  let prog :: (F_BN128, (F_BN128, F_BN128)) -> F_BN128
--      prog (x, (y, z)) =
--        let p = Point x y
--            atom :: Atom
--            atom = Atom z p 1
--            f :: Atom -> F_BN128 -> Atom
--            f a n = a & elem +~ n
--            ls = 1 ::: 2 ::: 3 ::: VNil
--         in foldl f atom ls ^. elem
--      compiledProg :: Stalk F_BN128 (F_BN128, (F_BN128, F_BN128)) F_BN128
--      compiledProg = Categorify.expression prog
--   in do
--        x <- Snarkl.fresh_input
--        y <- Snarkl.fresh_input
--        z <- Snarkl.fresh_input
--        yz <- pair y z
--        p <- pair x yz
--        runStalk compiledProg p
