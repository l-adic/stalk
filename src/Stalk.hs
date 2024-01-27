{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

-- | A category for Snarkl
module Stalk
  ( Stalk (..),
    SnarklTy,
  )
where

import Categorifier.Category
  ( UnsafeCoerceCat (..),
  )
import qualified Categorifier.Category as Categorifier
import Categorifier.Vec.Client ()
import qualified ConCat.Additive
import qualified ConCat.Category as ConCat
import Data.Bool (bool)
import qualified Data.Constraint as Constraint
import Data.Field.Galois (GaloisField)
import Data.Functor.Const (Const (..))
import Data.Proxy (Proxy (..))
import Data.Type.Nat (Nat (..))
import qualified Data.Type.Nat as Nat
import Data.Typeable (Typeable)
import Data.Vec.Lazy (Vec (..))
import GHC.Natural (Natural)
import Snarkl.Field (F_BN128)
import Snarkl.Language.Prelude (return, (>>), (>>=))
import qualified Snarkl.Language.Prelude as Snarkl
import Prelude (Bool, Either, error, fromInteger, ($), (+), (.))

-- | Mapping from Haskell types to Snarkl types.
data Stalk k a b = Stalk
  {runStalk :: Snarkl.TExp (SnarklTy a) k -> Snarkl.Comp (SnarklTy b) k}

instance
  (Nat.SNatI n, LiftSnarklTy a k, GaloisField k, Typeable (SnarklTy a)) =>
  Categorifier.RepCat (Stalk k) (Vec ('S ('S n)) a) (a, Vec ('S n) a)
  where
  abstC = Stalk $ \p -> do
    let last = Nat.reflectToNum (Proxy @n)
    a <- Snarkl.arr $ last + 2
    Snarkl.fst_pair p >>= Snarkl.set (a, 0)
    Snarkl.snd_pair p
      >>= \a' ->
        Snarkl.forall [0 .. last] $
          \i -> Snarkl.get (a', i) >>= Snarkl.set (a, i + 1)
    return a
  reprC = Stalk $ \a -> do
    let last = Nat.reflectToNum (Proxy :: Proxy n)
    let subLen = last + 1
    h <- Snarkl.get (a, 0)
    t <- Snarkl.arr subLen
    Snarkl.forall [0 .. last] (\i -> Snarkl.get (a, i + 1) >>= Snarkl.set (t, i))
    Snarkl.pair h t

instance
  (LiftSnarklTy k a, GaloisField k) =>
  Categorifier.RepCat (Stalk k) (Vec ('S 'Z) a) (a, Vec 'Z a)
  where
  abstC = Stalk $ \p -> do
    a <- Snarkl.arr 1
    Snarkl.fst_pair p >>= Snarkl.set (a, 0)
    return a
  reprC = Stalk $ \a -> do
    h <- Snarkl.get (a, 0)
    Snarkl.pair h Snarkl.unit

instance (LiftSnarklTy k a) => Categorifier.RepCat (Stalk k) (Vec 'Z a) () where
  abstC = Stalk return
  reprC = Stalk return

type family SnarklTy a :: Snarkl.Ty

type instance SnarklTy () = 'Snarkl.TUnit

type instance SnarklTy (a, b) = 'Snarkl.TProd (SnarklTy a) (SnarklTy b)

-- type instance SnarklTy (a -> b) = 'Snarkl.TMu TF
type instance SnarklTy Bool = 'Snarkl.TBool

type instance SnarklTy (Either a b) = 'Snarkl.TSum (SnarklTy a) (SnarklTy b)

type instance SnarklTy F_BN128 = 'Snarkl.TField

type instance SnarklTy Natural = 'Snarkl.TField

type instance SnarklTy (Vec ('S n) a) = 'Snarkl.TArr (SnarklTy a)

-- | Snarkl only supports non-empty sequences, but this exists for `Categorifier.RepCat`.
type instance SnarklTy (Vec 'Z a) = 'Snarkl.TUnit

type instance SnarklTy (a -> b) = 'Snarkl.TFun (SnarklTy a) (SnarklTy b)

type instance SnarklTy (Const k b) = SnarklTy k

instance (SnarklTy a ~ SnarklTy b) => Categorifier.UnsafeCoerceCat (Stalk k) a b where
  unsafeCoerceK = Stalk return

-- | This class exists because we can`t do @`Typeable` . `SnarklTy`@. It has a
--   universal instance.
class (Snarkl.Derive (SnarklTy a) k, Typeable (SnarklTy a), Snarkl.Zippable (SnarklTy a) k) => LiftSnarklTy k a

instance (Snarkl.Derive (SnarklTy a) k, Typeable (SnarklTy a), Snarkl.Zippable (SnarklTy a) k) => LiftSnarklTy k a

instance (GaloisField k) => ConCat.OpCon (ConCat.Coprod Stalk) (ConCat.Sat (LiftSnarklTy k)) where
  inOp = ConCat.Entail (Constraint.Sub Constraint.Dict)

instance ConCat.OpCon (ConCat.Exp Stalk) (ConCat.Sat (LiftSnarklTy k)) where
  inOp = ConCat.Entail (Constraint.Sub Constraint.Dict)

instance (GaloisField k) => ConCat.OpCon (ConCat.Prod (Stalk k)) (ConCat.Sat (LiftSnarklTy k)) where
  inOp = ConCat.Entail (Constraint.Sub Constraint.Dict)

instance ConCat.Category (Stalk k) where
  type Ok (Stalk k) = LiftSnarklTy k -- <whatever class SnarklTy ends up in, probably>
  id = Stalk Snarkl.return
  Stalk f . Stalk g = Stalk $ \x -> g x Snarkl.>>= f

instance (GaloisField k) => ConCat.ClosedCat (Stalk k) where
  -- curry :: (TExp ('TProd a b) -> Comp c) -> (TExp a -> Comp ('Mu TF))
  curry :: (ConCat.Ok3 (Stalk k) a b c) => Stalk k (ConCat.Prod Stalk a b) c -> Stalk k a (ConCat.Exp (Stalk k) b c)
  curry (Stalk f) = Stalk $ Snarkl.curry f
  uncurry :: (ConCat.Ok3 (Stalk k) a b c) => Stalk k a (ConCat.Exp (Stalk k) b c) -> Stalk k (ConCat.Prod (Stalk k) a b) c
  uncurry (Stalk f) = Stalk $ Snarkl.uncurry f

instance (GaloisField k) => ConCat.CoproductCat (Stalk k) where
  inl :: (ConCat.Ok2 (Stalk k) a b) => Stalk k a (ConCat.Coprod (Stalk k) a b)
  inl = Stalk Snarkl.inl
  inr = Stalk Snarkl.inr
  jam = Stalk $ Snarkl.case_sum Snarkl.return Snarkl.return

instance (GaloisField k) => ConCat.MonoidalPCat (Stalk k) where
  Stalk f *** Stalk g = Stalk $ \p -> do
    a <- Snarkl.fst_pair p Snarkl.>>= f
    b <- Snarkl.snd_pair p Snarkl.>>= g
    Snarkl.pair a b

instance (GaloisField k) => ConCat.MonoidalSCat (Stalk k) where
  Stalk f +++ Stalk g =
    Stalk $
      Snarkl.case_sum
        (\a -> f a Snarkl.>>= Snarkl.inl)
        (\b -> g b Snarkl.>>= Snarkl.inr)

instance (GaloisField k) => ConCat.ProductCat (Stalk k) where
  exl = Stalk Snarkl.fst_pair
  exr = Stalk Snarkl.snd_pair
  dup = Stalk $ \x -> Snarkl.pair x x

instance (GaloisField k) => ConCat.BraidedPCat (Stalk k)

instance (GaloisField k) => ConCat.DistribCat (Stalk k) where
  distl = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    Snarkl.snd_pair p
      >>= Snarkl.case_sum
        (\u -> Snarkl.pair a u >>= Snarkl.inl)
        (\v -> Snarkl.pair a v >>= Snarkl.inr)

instance (GaloisField k) => ConCat.BoolCat (Stalk k) where
  notC = Stalk $ Snarkl.return . Snarkl.not
  andC = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.&& b
  orC = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return . Snarkl.not $ Snarkl.not a Snarkl.&& Snarkl.not b
  xorC = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ Snarkl.xor a b

instance ConCat.TerminalCat (Stalk k)

instance (GaloisField k) => ConCat.ConstCat (Stalk k) Bool where
  const = Stalk . bool (\_ -> return Snarkl.false) (\_ -> return Snarkl.true)

instance (GaloisField k, SnarklTy k ~ 'Snarkl.TField) => ConCat.ConstCat (Stalk k) k where
  const a = Stalk $ \_ -> return $ Snarkl.fromField a

instance ConCat.ConstCat (Stalk k) () where
  const () = Stalk $ \_ -> return Snarkl.unit

instance (ConCat.Additive.Additive k, Nat.SNatI n, GaloisField k, SnarklTy k ~ 'Snarkl.TField) => ConCat.AddCat (Stalk k) (Vec ('S n)) k where
  sumAC = Stalk $ \a ->
    Snarkl.iterM
      (Nat.reflectToNum (Proxy :: Proxy n))
      (\n' e -> Snarkl.get (a, n') >>= (return . (Snarkl.+ e)))
      (Snarkl.fromField 0)

instance (GaloisField k) => ConCat.Additive.Additive k where
  zero = 0
  (^+^) = (+)

instance
  (ConCat.Ok (Stalk k) a, Snarkl.Zippable (SnarklTy a) k, GaloisField k) =>
  ConCat.IfCat (Stalk k) a
  where
  ifC = Stalk $ \p -> do
    b <- Snarkl.snd_pair p
    Snarkl.ifThenElse
      (Snarkl.fst_pair p)
      (Snarkl.fst_pair b)
      (Snarkl.snd_pair b)

instance (GaloisField k, SnarklTy k ~ 'Snarkl.TField) => ConCat.NumCat (Stalk k) k where
  addC = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.+ b
  mulC = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.* b
  subC = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ a Snarkl.- b
  negateC = Stalk $ Snarkl.return . Snarkl.negate
  powIC = error "no exponents"

instance (GaloisField k, SnarklTy a ~ 'Snarkl.TField) => ConCat.EqCat (Stalk k) a where
  equal = Stalk $ \p -> do
    a <- Snarkl.fst_pair p
    b <- Snarkl.snd_pair p
    Snarkl.return $ Snarkl.eq a b

cat ::
  (Typeable (SnarklTy a)) =>
  (Typeable (SnarklTy b)) =>
  Snarkl.TExp (SnarklTy (a -> b)) k ->
  Stalk k a b
cat f = Stalk $ \x -> Snarkl.apply f x

lowerCat ::
  (Typeable (SnarklTy a)) =>
  (Typeable (SnarklTy b)) =>
  Stalk k a b ->
  Snarkl.Comp (SnarklTy (a -> b)) k
lowerCat (Stalk f) = Snarkl.lambda f
