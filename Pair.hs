data Expr = LitI Int
            | LitB Bool
            | Pair Expr Expr
            | Fst Expr
            | Snd Expr
            | Swap Expr
        deriving (Eq,Show)

data Value = I Int
            | B Bool
            | P Value Value
        deriving (Eq,Show)

-- type domain = Maybe Value

sem :: Expr -> Maybe Value
sem (LitI i)    = Just (I i)

sem (LitB b)    = Just (B b)
sem (Pair l r)  = case (sem l, sem r) of
                    (Just lv, Just rv) -> Just (P lv rv) 
                    _ -> Nothing
sem (Fst e)     = case sem e of
                    Just ( P fv _) -> Just fv
                    _ -> Nothing
sem (Snd e)     = case sem e of
    Just ( P _ sv) -> Just sv
    _ -> Nothing
sem (Swap e)    = case sem e of
                    Just (P fv sv) -> Just (P sv fv)
                    _ -> Nothing


-- data types structure
data Type = TInt
          | TBool
          | TPair Type Type
    deriving (Eq,Show)
    

typeOf :: Expr -> Maybe Type
typeOf (LitI _) = Just TInt
typeOf (LitB _) = Just TBool
typeOf (Pair l r) = case (typeOf l, typeOf r) of
                (Just tl, Just tr) -> Just (TPair tl tr)
                _ -> Nothing
typeOf (Fst e)  = case typeOf e of
                    Just (TPair tf ts) -> Just tf
                    _ -> Nothing
typeOf (Snd e)  = case typeOf e of
                    Just (TPair tf ts) -> Just ts
                    _ -> Nothing
typeOf (Swap e) = case typeOf e of
                    Just (TPair tf ts) -> Just (TPair ts tf)
                    _ -> Nothing



-- -- type correct semantics
-- sem' :: Expr -> Value
-- sem' (LitI i)   = I i
-- sem' (LitB b)   = B b
-- sem' (Pair l r) = P (sem' l) (sem' r)
-- sem' (Fst e)    = case sem' e of
--                     P l r -> l
--                     _ -> error "internal error"  
-- sem' (Snd e)    = case sem' e of
--                     P l r -> r
--                     _ -> error "internal error"
-- sem' (Swap e)   = case sem' e of
--     P l r -> P r l
--     _ -> error "internal error"

-- type correct semantics
sem' :: Expr -> Value
sem' (LitI i)   = I i
sem' (LitB b)   = B b
sem' (Pair l r) = P (sem' l) (sem' r)
sem' (Fst e)    = fst (evalPair e) 
sem' (Snd e)    = snd (evalPair e)
sem' (Swap e)   = P (snd p) (fst p)
    where p = evalPair e

-- evaluate expr to pair helper
evalPair :: Expr -> (Value,Value)
evalPair e = case sem' e of
            P l r -> (l,r)
            _ -> error "internal error"