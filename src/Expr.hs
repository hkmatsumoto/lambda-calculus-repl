module Expr
    ( Expr(..)
    )
where

data Expr
    = Var Char
    | Abs Expr Expr
    | App Expr Expr

instance Show Expr
  where
    show (Var x    ) = [x]
    show (Abs x  t1) = "(\\" ++ show x ++ ". " ++ show t1 ++ ")"
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
