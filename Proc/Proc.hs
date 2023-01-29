module Proc where
  {-
    Proc is an extension of the While language with support for procedures.
    Procedures can be created in 'begin ... end' blocks with the 'proc p is' syntax, 
    and they are called using 'call p'.
    Proc also distinguishes between local and global variables
    Proc uses static scope rules as opposed to dynamic scoping.
  -}
  import Store ( Var, Z )
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

  data VarDef = Var ::= AExp deriving Show
  data Dv = Dv VarDef Dv | NilV deriving Show
  data Dp = Proc String Stmt Dp | NilP deriving Show

  -- Statements can be constructed as such
  data Stmt = Var := AExp
            | Skip | Stmt :\ Stmt
            | If BExp Stmt Stmt
            | While BExp Stmt
            | Begin Dv Dp Stmt
            | Call String
            | End
                            deriving Show

  iter :: Integer -> (x -> x) -> x -> x
  iter 0 f x0 = x0 
  iter k f x0 = f (iter (k-1) f x0)

  fix :: (x -> x) -> x -> x
  fix = iter 1000