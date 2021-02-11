{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

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


deriving instance Show (ProgramTerm t)
