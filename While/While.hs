module While where
  {-
    While is a simple language supporting if statements and while loops.
    It consist of arithmetic - and boolean expressions, combined using statements
  -}
  import State ( Var, Z )
  -- Arithmetic expressions can be constructed as such
  data AExp = N Z | V Var
            | AExp :+ AExp
            | AExp :- AExp
            | AExp :* AExp
                            deriving Show

  -- Boolean expressions can be constructed as such
  data BExp = TT | FF | Neg BExp | BExp :& BExp | BExp :| BExp
            | AExp :== AExp | AExp :< AExp
                            deriving Show

  -- Statements can be constructed as such
  data Stmt = Var := AExp
            | Skip | Stmt :\ Stmt
            | If BExp Stmt Stmt
            | While BExp Stmt
                            deriving Show

  iter :: Integer -> (x -> x) -> x -> x
  iter 0 f x0 = x0 
  iter k f x0 = f (iter (k-1) f x0)

  fix :: (x -> x) -> x -> x
  fix = iter 1000