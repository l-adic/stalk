{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Examples.Arithmetic (simpleBool, simpleArith) where

import qualified Categorifier.Categorify as Categorify
import Categorifier.Vec.Client ()
import Snarkl.Field (F_BN128)
import Snarkl.Language (Ty (..), pair, (>>=))
import qualified Snarkl.Language.SyntaxMonad as Snarkl
import Straw (Straw (runStraw))
import Prelude (Bool (..), fromInteger, (*), (+), (-), (==))

-- we need the because we enabled rebindable syntax
ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

{-
pub z: F;
let x = 4^2;
let y = 2 * x * z - 1 ;
x + y
-}
simpleArith :: Snarkl.Comp 'TField F_BN128
simpleArith =
  let prog :: F_BN128 -> F_BN128
      prog z =
        let x = 4 * 4
            y = 2 * x * z - 1
         in x + 1
      compiledProg :: Straw F_BN128 F_BN128 F_BN128
      compiledProg = Categorify.expression prog
   in do
        z <- Snarkl.fresh_input
        runStraw compiledProg z

{-
pub x: F;
pub y: F;
let f = x^2  + 2*x + 1 - y;
let g = x + 7 - y;
if (f == 0 && g == 0)
  then 42
  else 0
-}

simpleBool :: Snarkl.Comp 'TField F_BN128
simpleBool =
  let prog :: (F_BN128, F_BN128) -> F_BN128
      prog (x, y) =
        let -- f = x * x + 2 * x + 1 - y
            g = x + 7 - y
         in if g == 0 then 42 else 0

      compiledProg :: Straw F_BN128 (F_BN128, F_BN128) F_BN128
      compiledProg = Categorify.expression prog
   in do
        x <- Snarkl.fresh_input
        y <- Snarkl.fresh_input
        p <- pair x y
        runStraw compiledProg p
