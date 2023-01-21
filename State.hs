{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs #-}

module State where

type Var = String
type Z = Integer

-- The State is simply a function from variables to integers
type State = Var -> Z

-- We have 3 variables in our toy While language
x :: [Char]
x = "x"
y :: [Char]
y = "y"
z :: [Char]
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