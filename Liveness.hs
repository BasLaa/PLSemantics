{-# OPTIONS_GHC -XTypeSynonymInstances -XFlexibleInstances #-}

module Liveness where

import State
import While

-- Liveness analysis of the While language
-- liveness determines whether a variables is live or dead

-- A live variable is a variable whose information is required
-- A dead variable is a variable that can safely be forgotten

-- Complete lattices

data P = Dead | Live deriving Show

bot :: P
bot = Dead

top :: P
top = Live

lub :: P -> P -> P
Dead `lub` c = c
c `lub` Dead = c
Live `lub` Live = Live

type PState = Var -> P

botS :: PState
botS x = bot

lubS :: PState -> PState -> PState 
(ps0 `lubS` ps1) x = ps0 x `lub` ps1 x


-- Analysis

-- Analysis of arithmetic expressions
liveA :: AExp -> P -> PState
liveA (N n) p  = botS
liveA (V x) p  = upd botS x p
liveA (a0 :+ a1) p = liveA a0 p `lubS` liveA a1 p 
liveA (a0 :- a1) p = liveA a0 p `lubS` liveA a1 p 
liveA (a0 :* a1) p = liveA a0 p `lubS` liveA a1 p 

-- Analysis of boolean expressions
liveB :: BExp -> P -> PState
liveB TT p = botS
liveB FF p = botS
liveB (Neg b) p = liveB b p
liveB (b0 :& b1)  p = liveB b0 p `lubS` liveB b1 p
liveB (b0 :| b1)  p = liveB b0 p `lubS` liveB b1 p
liveB (a0 :== a1) p = liveA a0 p `lubS` liveA a1 p
liveB (a0 :< a1)  p = liveA a0 p `lubS` liveA a1 p


-- Analysis of statements
cond :: (P -> PState) -> (PState -> PState) -> (PState -> PState)
                                                -> (PState -> PState)
cond p f g ps = (f ps `lubS` g ps) `lubS` p top


liveS :: Stmt -> PState -> PState
liveS (x := a) = \ ps -> upd ps x Dead `lubS` liveA a (lkp ps x) 
liveS Skip = id
liveS (s0 :\ s1) = liveS s0 . liveS s1
liveS (If b s0 s1)  
    = cond (liveB b) (liveS s0) (liveS s1)
liveS (While b s0)
    = fix f (const botS) where
        f g = cond (liveB b) (liveS s0 . g) id


-- Examples

swap = ((z := V x) :\ (x := V y)) :\ (y := V z)


test = (x := N 3) :\ (If (V z :== N 17) (y := (V x :+ N 2)) (y := N 5))


fact = (y := N 1) :\
         While (Neg (V x :== N 1))
            ((y := (V x :* V y)) :\ (x := (V x :- N 1)))


