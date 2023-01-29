module Secinfflow where
  import While
  import State

  -- Secure information flow analysis analyzes which variables 
  -- hold relevant security information

  history = "history"

  -- Complete lattices

  data Class = Low | High deriving (Eq, Show)

  bot :: Class
  bot = Low

  top :: Class
  top = High

  lub :: Class -> Class -> Class
  Low `lub` c = c
  c `lub` Low = c
  High `lub` High = High

  type PState = Var -> Class

  botS :: PState
  botS x = bot

  topS :: PState
  topS x = top

  lubS :: PState -> PState -> PState 
  (ps0 `lubS` ps1) x = ps0 x `lub` ps1 x


  -- Analysis

  sifA :: AExp -> PState -> Class
  --{- uncomment this for a more refined version
  sifA a ps | lkp ps history == High = High
  ---}
  sifA (N n) ps = Low 
  sifA (V x) ps = lkp ps x 
  sifA (a0 :+ a1) ps = sifA a0 ps `lub` sifA a1 ps 
  sifA (a0 :- a1) ps = sifA a0 ps `lub` sifA a1 ps 
  sifA (a0 :* a1) ps = sifA a0 ps `lub` sifA a1 ps 


  sifB :: BExp -> PState -> Class
  --{- uncomment this for a more refined version
  sifB b ps | lkp ps history == High = High
  ---}
  sifB TT ps = Low  
  sifB FF ps = Low 
  sifB (Neg b) ps = sifB b ps
  sifB (b0 :& b1)  ps = sifB b0 ps `lub` sifB b1 ps
  sifB (b0 :| b1)  ps = sifB b0 ps `lub` sifB b1 ps
  sifB (a0 :== a1) ps = sifA a0 ps `lub` sifA a1 ps
  sifB (a0 :< a1)  ps = sifA a0 ps `lub` sifA a1 ps


  condP :: (PState -> Class) -> (PState -> PState) -> (PState -> PState)
                                                  -> (PState -> PState)
  condP p f g ps = case p ps of 
      Low  -> f ps `lubS` g ps 
      High -> -- topS
              --{- comment the above and uncomment this for a more refined version
                upd (f ps' `lubS` g ps') history h
                      where h   = lkp ps history
                            ps' = upd ps history High
              ---}

  sifS :: Stmt -> PState -> PState 
  sifS (x := a) = \ ps -> upd ps x (sifA a ps) 
  sifS Skip = id
  sifS (s0 :\ s1) = sifS s1 . sifS s0
  sifS (If b s0 s1)  
      = condP (sifB b) (sifS s0) (sifS s1)
  sifS (While b s0)
      = fix f (\ _ -> botS) where
          f g = condP (sifB b) (g . sifS s0) id



  -- Examples

  swap = ((z := V x) :\ (x := V y)) :\ (y := V z)


  test = (x := N 3) :\ (If (V z :== N 17) (x := (V x :+ N 2)) (y := N 5))


  fact = (y := N 1) :\
          While (Neg (V x :== N 1))
              ((y := (V x :* V y)) :\ (x := (V x :- N 1)))

