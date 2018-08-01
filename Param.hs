-- | Illustration of static vs. dynamic scope and
--   several different parameter passing schemes.
module Param where


--
-- * Abstract syntax
--

-- | Variable names.
type Var = String

-- | Abstract syntax.
data Exp = Lit Int          -- integer literal
         | Add Exp Exp      -- addition expression
         | Let Var Exp Exp  -- variable binding
         | Ref Var          -- variable reference
         | Fun Var Exp      -- anonymous function w/ one argument
         | App Exp Exp      -- function application
  deriving (Eq,Show)


-- ** Example programs


-- | Example program that defines the successor function then uses it.
--  
--   let succ = \x -> x+1
--   in succ (succ 5)
-- (\x -> x+1) 5

exSucc :: Exp
exSucc = Let "succ" (Fun "x" (Add (Ref "x") (Lit 1)))
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))


-- | Example program that illustrates the difference between
--   static and dynamic scope. Is the result 12 or 13?
--   Static = 12, Dynamic = 13
--
--   let z = 2 in
--   let f = (\x -> x+z) in
--   let z = 3 in
--   f 10
--
exScope :: Exp
exScope = Let "z" (Lit 2)
              (Let "f" (Fun "x" (Add (Ref "x") (Ref "z")))
                   (Let "z" (Lit 3)
                        (App (Ref "f") (Lit 10))))


-- ex :: Exp
-- ex = Let "z" (Lit 2)
--      (Let "f" (Fun "x" (Add (Ref "x") (Ref "z")))
--          "z" (Lit 4))
--       (Let "z" (Lit 3))
--       (App (Ref "f") (Lit 10))
--
-- * Various semantics
--
--   Illustrating different scoping and parameter passing schemes.
--

-- | An environment maps variables to some type of values.
type Env a = [(Var,a)]


-- ** Dynamic scoping, call-by-value

-- | Values.
data DVal = DI Int      -- integers
          | DF Var Exp  -- functions
  deriving (Eq,Show)


-- | Semantic function
dsem :: Exp -> Env DVal -> Maybe DVal
dsem (Lit i)     m = Just (DI i)
dsem (Add l r)   m = case (dsem l m, dsem r m) of
                      (Just (DI i), Just (DI j)) -> Just (DI (i + j))
                      _ -> Nothing
dsem (Let v b e) m = case (dsem b m) of 
                      (Just db) -> dsem e ((v, db) : m)
                      _ -> Nothing
dsem (Ref v)     m = lookup v m
dsem (Fun v e)   m = Just (DF v e)
dsem (App l r) m = case (dsem l m, dsem r m) of
                    (Just (DF v e), Just dr) -> dsem e ((v,dr) : m) 
                    _ -> Nothing

-- ** Static scoping, call-by-value

-- | A value is either an integer or a closure.
data SVal = SI Int                  -- integer
          | SC (Env SVal) Var Exp   -- closure
          deriving (Eq, Show)

-- | Semantic function.
ssem :: Exp -> Env SVal -> Maybe SVal
ssem (Lit i)     m = Just (SI i)
ssem (Add l r)   m = case (ssem l m, ssem r m) of
                      (Just (SI i), Just (SI j)) -> Just (SI (i + j))
                      _ -> Nothing
ssem (Let v b e) m = case (ssem b m) of 
                      (Just sv) -> ssem e ((v, sv) : m)
                      _ -> Nothing
ssem (Ref v)     m = lookup v m
ssem (Fun v e)   m = Just (SC m v e)
ssem (App l r)   m = case (ssem l m, ssem r m) of
    (Just (SC m' v e), Just sr) -> ssem e ((v,sr):m')
    _ -> Nothing

-- ** Static scoping, call-by-name

-- | An expression that loops forever if evaluated.


-- | Return 3 no matter the argument.


-- | An example where call-by-value loops and call-by-name terminates.
--
--   You can test this in GHCi as follows:
--   
--   > nsem exLoop []
--   Just (NI 3)
--
--   > ssem exLoop []
--   (use Ctrl-C to terminate, once you get bored)
--

-- | Statically scoped (closed) unevaluated expressions.
data SExp = SE (Env SExp) Exp
  deriving (Eq,Show)

-- | A value is either an integer or a closure.
data NVal = NI Int                  -- integer
          | NC (Env SExp) Var Exp   -- closure
  deriving (Eq,Show)

-- | Semantic function. Note that our environments now contain unevaluated
--   expressions rather than values! This is the key difference from
--   call-by-value. Note that we do not evaluate 'b' in the Let case or 'r'
--   in the App case, but rather just close up the expression and put it in
--   the environment. Only when we use a variable (Ref case), do we evaluate
--   it. This means that each argument to a function will be evaluated as
--   many times as it is used when evaluating the body of the function.
nsem :: Exp -> Env SExp -> Maybe NVal
nsem (Lit i)     m = Just (NI i)
nsem (Add l r)   m = case (nsem l m, nsem r m) of
                     (Just (NI i), Just (NI j)) -> Just (NI (i+j))
                     _ -> Nothing
-- nsem (Let v b e) m = nsem e ((v, SE m b):m)
nsem (Let v b e) m = case nsem b m of
                     Just nv -> nsem e ((v, SE m b):m)
                     _ -> Nothing
nsem (Ref v)     m = case lookup v m of
                     Just (SE m' e) -> (nsem e m')
                     _ -> Nothing
nsem (Fun v e)   m = Just (NC m v e)
nsem (App l r)   m = case nsem l m of
                     Just (NC m' v e) -> nsem l ((v, SE m' r):m')
                     _ -> Nothing

-- ** Static scoping, call-by-need (lazy evaluation)

-- | A function that adds 0 to y a hundred thousand times.


-- | An example that illustrates the performance difference between
--   call-by-name and call-by-need. Neither will evaluate the argument
--   (slow applied to 2) until it is used, but call-by-name will
--   evaluate it 16 times in the body of the times16 function, while
--   call-by-need will evaluate it once and then cache the result.
--
--   To observe the difference yourself, you can run:
--
--   > name exSlow
--   Just (NI 32)   -- a few seconds later
--
--   > lazy exSlow
--   Just (NI 32)   -- instantly
--
--   You can also confirm that lazy evaluation *does* terminate on the
--   loop example. It's the best of both worlds!
--
--   > lazy exLoop
--   Just (NI 3)
--   


-- | Environment for lazy evaluation. Note that each name in the environment
--   is bound to either an unevaluated expression or a value (if it has
--   already been evaluated).
type LEnv = Env (Either LExp LVal)

-- | Statically scoped (closed) expressions.
data LExp = LE LEnv Exp
  deriving (Eq,Show)
     
-- | A value is either an integer or a closure.
data LVal = LI Int            -- integer
          | LC LEnv Var Exp   -- closure
  deriving (Eq,Show)

-- | Semantic function. Note that our semantic domain now reflects the fact
--   that evaluating an expression can update the environment! The first
--   time we reference a variable, we may have to do some evaluation (this
--   corresponds to the Left case in Ref), but then to remember that we have
--   evaluated it we have to return an updated environment that contains a
--   mapping from x to the evaluated value. Also observe that when evaluating
--   an expression like Add, we must thread the environment through the
--   evaluation of the two subexpressions, since evaluating each one could
--   change it.
lsem :: Exp -> LEnv -> Maybe (LEnv, LVal)
lsem (Lit i)     m = Just (m, LI i)
lsem (Add l r)   m = case lsem l m of
                       Just (m', LI i) ->
                         case lsem r m' of
                           Just (m'', LI j) -> Just (m'', LI (i+j))
                           _ -> Nothing
                       _ -> Nothing
lsem (Let x l r) m = lsem r ((x, Left (LE m l)):m)
lsem (Ref x)     m = case lookup x m of
                       Just (Left (LE m' e)) ->
                         case lsem e m' of
                           Just (_,v) -> Just ((x,Right v):m, v)
                           _ -> Nothing
                       Just (Right v) -> Just (m,v)
                       _ -> Nothing
lsem (Fun x e)   m = Just (m, LC m x e)
lsem (App l r)   m = case lsem l m of
                       Just (m', LC mb x b) -> lsem b ((x, Left (LE m' r)):mb)
                       _ -> Nothing

-- | Evaluate with call-by-name evaluation and an empty environment.
name :: Exp -> Maybe NVal
name e = nsem e []

-- | Evaluate with lazy evaluation and an empty environment.
lazy :: Exp -> Maybe LVal
lazy e = fmap snd (lsem e [])
