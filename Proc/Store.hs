{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs #-}

module Store where

-- To be able to define environments, we move from a simple State
-- to a more advanced Store, which maps variables to a store location
-- and that location in the storage holds a value (integer)
type Var = String
type Z = Integer
type Loc = Z

-- The State is simply a function from variables to integers
type State = Var -> Z

type Store = Loc -> Z
type Envv = Var -> Loc

-- We can get the simple lookup function back by composing our environment and store
lookup :: Envv -> Store -> State
lookup e sto = sto . e

updSto :: Store -> Loc -> Z -> (Loc -> Z)
updSto sto nl z ol = if ol == nl then z else sto ol

-- We have 3 variables in our toy Proc language
x :: Var
x = "x"
y :: Var
y = "y"
z :: Var
z = "z"

makeState :: [(Var,a)] -> a -> (Var -> a)
makeState [] z0 = const z0
makeState ((v,z):dict) z0
    = \ x -> if x == v then z else makeState dict z0 x

lkp :: (Var -> a) -> Var -> a
lkp s = s

upd :: (Var -> a) -> Var -> a -> (Var -> a)
upd s v z x = if x == v then z else s x

instance Show x => Show (Var -> x) where
    show :: Show x => (Var -> x) -> String
    show = showState

showState :: Show x => (Var -> x) -> String
showState s = show [(x, lkp s x), (y, lkp s y), (z, lkp s z)]