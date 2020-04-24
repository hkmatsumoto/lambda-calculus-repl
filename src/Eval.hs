module Eval
    ( eval
    )
where

import           Data.Set                       ( Set
                                                , (\\)
                                                , intersection
                                                , singleton
                                                , notMember
                                                )

import           Expr

eval :: Expr -> Expr
eval expr = case eval' expr of
    Nothing   -> expr
    Just expr -> eval expr

eval' :: Expr -> Maybe Expr
eval' (App (Abs (Var x) t12) v2) | isValue v2 = substitute x v2 t12
eval' (App v1 t2) | isValue v1                = App v1 <$> eval' t2
eval' (App t1 t2)                             = flip App t2 <$> eval' t1
eval' _                                       = Nothing

isValue :: Expr -> Bool
isValue (Abs _ _) = True
isValue _         = False

-- TODO: make this func return Expr (cf. TaPL 54P)
substitute :: Char -> Expr -> Expr -> Maybe Expr
substitute x s (Var y) | x == y = return s
substitute x s (Var y)          = return $ Var y
substitute x s (Abs (Var y) t1) | y `notMember` freeVariables s =
    Abs (Var y) <$> substitute x s t1
substitute x s (App t1 t2) = do
    t1 <- substitute x s t1
    t2 <- substitute x s t2
    return $ App t1 t2
substitute _ _ _ = Nothing

freeVariables :: Expr -> Set Char
freeVariables (Var x    ) = singleton x
freeVariables (Abs x  t1) = freeVariables t1 \\ freeVariables x
freeVariables (App t1 t2) = freeVariables t1 `intersection` freeVariables t2
