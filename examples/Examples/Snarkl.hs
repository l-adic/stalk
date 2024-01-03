{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Examples.Snarkl
  ( arr_ex,
    p1,
  )
where

import Snarkl.Field (F_BN128)
import Snarkl.Language (Comp, TExp, Ty (..), arr, forall, fromField, get, return, set, (+), (>>), (>>=))
import Prelude (fromInteger, ($))

arr_ex :: TExp 'TField F_BN128 -> Comp 'TField F_BN128
arr_ex x = do
  a <- arr 2
  forall [0 .. 1] (\i -> set (a, i) x)
  y <- get (a, 0)
  z <- get (a, 1)
  return $ y + z

p1 = arr_ex (fromField 1)
