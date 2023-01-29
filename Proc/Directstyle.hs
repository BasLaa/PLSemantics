module Directstyle where
import Store
import Proc

-- Direct-Style Denotational Semantics of the While language

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
cond :: (Store -> Bool) -> (Store -> Store) -> (Store -> Store) -> (Store -> Store)
cond p g1 g2 s = if p s then g1 s else g2 s

bot :: Store -> Store
bot = undefined

cS :: Stmt -> Envv -> (Store -> Store)
cS (x := a) env =
    let l = env x
    in \sto -> updSto sto l (cA a (Store.lookup env sto))
cS Skip _  = id
cS (s0 :\ s1) env = cS s1 env . cS s0 env
cS (If b s0 s1) env
    = cond (cB b . Store.lookup env) (cS s0 env) (cS s1 env)
cS (While b s0) env
    = fix f bot where -- uses a fixed point
        f g = cond (cB b . Store.lookup env) (g . cS s0 env) id


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