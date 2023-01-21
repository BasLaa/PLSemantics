module Contstyle where
import State
import While

-- Continuation-Style Denotational Semantics of the While language
{-
  In continuation-style semantics, a continuation describes the effect
  of executing the remainder of the program.
  Whereas in direct-style, a state describes the current state of the program.
-}

type Cont = State -> State

-- The semantics of arithmetic expressions
cA :: AExp -> State -> Z
cA (N n) s = n
cA (V x) s = lkp s x
cA (a0 :+ a1) s = (+) (cA a0 s) (cA a1 s)
cA (a0 :- a1) s = (-) (cA a0 s) (cA a1 s)
cA (a0 :* a1) s = (*) (cA a0 s) (cA a1 s)

-- The semantics of boolean expressions
cB :: BExp -> State -> Bool
cB TT _ = True
cB FF _ = False
cB (Neg b) s = not (cB b s)
cB (b0 :& b1)  s = (&&) (cB b0 s) (cB b1 s)
cB (b0 :| b1)  s = (||) (cB b0 s) (cB b1 s)
cB (a0 :== a1) s = (==) (cA a0 s) (cA a1 s)
cB (a0 :< a1)  s = (<) (cA a0 s) (cA a1 s)

-- The semantics of statements

cond :: (State -> Bool) -> (State -> State) -> (State -> State) -> (State -> State)
cond p g1 g2 s = if p s then g1 s else g2 s

bot :: Cont -> Cont
bot = undefined

cS :: Stmt -> Cont -> Cont
cS (x := a) c s = c (upd s x (cA a s))
cS Skip c s = s
cS (s0 :\ s1) c s = (cS s0 c . cS s1 c) s -- statements executed in reverse order
cS (If b s0 s1) c s
    = cond (cB b) (cS s0 c) (cS s1 c) s
cS (While b s0) c s
    = fix f bot c s  where -- uses a fixed point
        f g c = cond (cB b) (cS s0 (g c)) c


-- Test programs

-- swaps x and y
swap :: Stmt
swap = ((z := V x) :\ (x := V y)) :\ (y := V z)


test :: Stmt
test = (x := N 3) :\ If (V z :== N 17) (y := (V x :+ N 3)) (y := N 5)

-- factorial of x in y
fact :: Stmt
fact = (y := N 1) :\
         While (Neg (V x :== N 1))
            ((y := (V x :* V y)) :\ (x := (V x :- N 1)))