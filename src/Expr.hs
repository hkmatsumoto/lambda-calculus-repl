module Expr
    ( Expr(..)
    )
where

data Expr
    = Var Char
    | Abs Expr Expr
    | App Expr Expr deriving (Show)
