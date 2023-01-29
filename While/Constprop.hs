{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}


module Constprop where

import State
import While

-- Constant propagation analysis analyzes whether variables have definite values in a program

-- Complete lattices

data Const = None | C Z | Any deriving Show

bot :: Const
bot = None

lub :: Const -> Const -> Const
Any `lub` _ = Any
_ `lub` Any = Any
C z0 `lub` C z1 = if z0 == z1 then C z0 else Any
None `lub` c = c
c `lub` None = c

data TT = NoneT | CT Bool | AnyT deriving Show


type PState = Var -> Const

botS :: PState
botS x = bot

lubS :: PState -> PState -> PState
(ps0 `lubS` ps1) x = ps0 x `lub` ps1 x

-- Analysis

absaop :: (Z -> Z -> Z) -> Const -> Const -> Const
absaop _ None _ = None
absaop _ _ None = None
absaop op (C z0) (C z1) = C (z0 `op` z1)
absaop _ Any _  = Any
absaop _ _ Any  = Any


cpA :: AExp -> PState -> Const
cpA (N n) ps = C n
cpA (V x) ps = lkp ps x
cpA (a0 :+ a1) ps = absaop (+) (cpA a0 ps) (cpA a1 ps)
cpA (a0 :- a1) ps = absaop (-) (cpA a0 ps) (cpA a1 ps)
cpA (a0 :* a1) ps = absaop (*) (cpA a0 ps) (cpA a1 ps)


abscomp :: (Z -> Z -> Bool) -> Const -> Const -> TT
abscomp _ None _ = NoneT
abscomp _ _ None = NoneT
abscomp op (C z0) (C z1) = CT (z0 `op` z1)
abscomp _ Any _  = AnyT
abscomp _ _  Any = AnyT


absbop :: (Bool -> Bool -> Bool) -> TT -> TT -> TT
absbop _ NoneT _ = NoneT
absbop _ _ NoneT = NoneT
absbop op (CT z0) (CT z1) = CT (z0 `op` z1)
absbop _ AnyT _  = AnyT
absbop _ _  AnyT = AnyT


cpB :: BExp -> PState -> TT
cpB TT _ = CT True
cpB FF _ = CT False
cpB (Neg b) ps = case cpB b ps of
                     NoneT -> NoneT
                     CT bv -> CT (not bv)
                     AnyT -> AnyT
cpB (b0 :& b1)  ps = absbop (&&) (cpB b0 ps) (cpB b1 ps)
cpB (b0 :| b1)  ps = absbop (||) (cpB b0 ps) (cpB b1 ps)
cpB (a0 :== a1) ps = abscomp (==) (cpA a0 ps) (cpA a1 ps)
cpB (a0 :< a1)  ps = abscomp (<) (cpA a0 ps) (cpA a1 ps)


cond :: (x -> TT) -> (x -> PState) -> (x -> PState) -> (x -> PState)
cond p f g ps = case p ps of
    NoneT    -> botS
    CT True  -> f ps
    CT False -> g ps
    AnyT     -> f ps `lubS` g ps


cpS :: Stmt -> PState -> PState
cpS (x := a) = \ s -> upd s x (cpA a s)
cpS Skip = id
cpS (s0 :\ s1) = cpS s1 . cpS s0
cpS (If b s0 s1)
    = cond (cpB b) (cpS s0) (cpS s1)
cpS (While b s0)
    = fix f (const botS) where
        f g = cond (cpB b) (g . cpS s0) id


swap = ((z := V x) :\ (x := V y)) :\ (y := V z)


test = (x := N 3) :\ (If (V z :== N 17) (y := (V x :+ N 2)) (y := N 5))


fact = (y := N 1) :\
         While (Neg (V x :== N 1))
            ((y := (V x :* V y)) :\ (x := (V x :- N 1)))
