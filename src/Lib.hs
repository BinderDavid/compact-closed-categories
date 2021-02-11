{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}

module Lib where

data ValType where
  Zero :: ValType
  One :: ValType
  (:+:) :: ValType -> ValType -> ValType
  (:*:) :: ValType -> ValType -> ValType
  deriving (Show, Eq, Ord)

data ValueTerm (t :: ValType) where
  TT :: ValueTerm One
  InjL :: ValueTerm t1 -> ValueTerm (t1 :+: t2)
  InjR :: ValueTerm t2 -> ValueTerm (t1 :+: t2)
  Pair :: ValueTerm t1 -> ValueTerm t2 -> ValueTerm (t1 :*: t2)

deriving instance Show (ValueTerm t)

data ProgramType where
  (:<->:) :: ValType -> ValType -> ProgramType

infix 1 :<->:

deriving instance Show ProgramType
deriving instance Eq ProgramType
deriving instance Ord ProgramType

data ProgramTerm (t :: ProgramType) where
  -- Category
  Id :: ProgramTerm (t1 :<->: t1)
  Compose :: ProgramTerm (t1 :<->: t2) -> ProgramTerm (t2 :<->: t3) -> ProgramTerm (t1 :<->: t3)
  -- Congruence
  CongSum :: ProgramTerm (t1 :<->: t2) -> ProgramTerm (t3 :<->: t4) -> ProgramTerm (t1 :+: t3 :<->: t2 :+: t4)
  CongProduct :: ProgramTerm (t1 :<->: t2) -> ProgramTerm (t3 :<->: t4) -> ProgramTerm (t1 :*: t3 :<->: t2 :*: t4)
  -- Sums
  UnitePlus :: ProgramTerm (Zero :+: t1 :<->: t1)
  UnitiPlus :: ProgramTerm (t1 :<->: Zero :+: t1)
  SwapPlus :: ProgramTerm (t1 :+: t2 :<->: t2 :+: t1)
  AssocPlusL :: ProgramTerm (t1 :+: (t2 :+: t3) :<->: (t1 :+: t2) :+: t3)
  AssocPlusR :: ProgramTerm ((t1 :+: t2) :+: t3 :<->: t1 :+: (t2 :+: t3))
  -- Products
  UniteTimes :: ProgramTerm (One :*: t1 :<->: t1)
  UnitiTimes :: ProgramTerm (t1 :<->: One :*: t1)
  SwapTimes :: ProgramTerm (t1 :*: t2 :<->: t2 :*: t1)
  AssocTimesL :: ProgramTerm (t1 :*: (t2 :*: t3) :<->: (t1 :*: t2) :*: t3)
  AssocTimesR :: ProgramTerm ((t1 :*: t2) :*: t3 :<->: t1 :*: (t2 :*: t3))
  -- Other
  AbsorbR :: ProgramTerm (Zero :*: t1 :<->: Zero)
  FactorzL :: ProgramTerm (Zero :<->: Zero :*: t1)
  Dist :: ProgramTerm ((t1 :+: t2) :*: t3 :<->: (t1 :*: t3) :+: (t2 :*: t3))
  Factor :: ProgramTerm ((t1 :*: t3) :+: (t2 :*: t3) :<->: (t1 :+: t2) :*: t3)

absurd :: ValueTerm Zero -> a
absurd tm = case tm of {}

deriving instance Show (ProgramTerm t)

delta :: ProgramTerm (t1 :<->: t2) -> ValueTerm t1 -> ValueTerm t2
-- Category
delta Id _ = error "delta is not defined on Id"
delta (Compose _ _) _ = error "delta is not defined on Compose"
-- Congruence
delta (CongSum _ _) _ = error "delta is not defined on CongSum"
delta (CongProduct _ _) _ = error "delta is not defined on CongProduct"
-- Sums
delta UnitePlus (InjR tm) = tm
delta UnitePlus (InjL tm) = absurd tm
delta UnitiPlus tm = InjR tm
delta SwapPlus (InjL tm) = InjR tm
delta SwapPlus (InjR tm) = InjL tm
delta AssocPlusL (InjL tm) = InjL (InjL tm)
delta AssocPlusL (InjR (InjL tm)) = InjL (InjR tm)
delta AssocPlusL (InjR (InjR tm)) = InjR tm
delta AssocPlusR (InjL (InjL tm)) = InjL tm
delta AssocPlusR (InjL (InjR tm)) = InjR (InjL tm)
delta AssocPlusR (InjR tm) = InjR (InjR tm)
-- Products
delta UniteTimes (Pair TT tm) = tm
delta UnitiTimes tm = Pair TT tm
delta SwapTimes (Pair tm1 tm2) = Pair tm2 tm1
delta AssocTimesL (Pair tm1 (Pair tm2 tm3)) = Pair (Pair tm1 tm2) tm3
delta AssocTimesR (Pair (Pair tm1 tm2) tm3) = Pair tm1 (Pair tm2 tm3)
-- Other
delta AbsorbR (Pair tm _) = absurd tm
delta Dist (Pair (InjL tm1) tm2) = InjL (Pair tm1 tm2)
delta Dist (Pair (InjR tm1) tm2) = InjR (Pair tm1 tm2)
delta Factor (InjL (Pair tm1 tm2)) = Pair (InjL tm1) tm2
delta Factor (InjR (Pair tm1 tm2)) = Pair (InjR tm1) tm2

