-- Robert Hayden Anderson
-- anderrob
-- anderrob@oregonstate.edu
-- cs381
-- summer 2018

module MiniLogo where

    import Data.List
    import Prelude hiding (Num)
    
    --
    -- * MiniLogo
    --
    -- | The grammar:
    --      num ::= (any natural number)
    --      var ::= (any variable name)
    --    macro ::= (any macro name)
    --
    --     prog ::= ε | cmd; prog                 sequence of commands
    --
    --     mode ::= up | down                     pen status
    --
    --     expr ::= var                           variable reference
    --           |  num                           literal number
    --           |  expr + expr                   addition expression
    --
    --      cmd ::= pen mode                      change pen status
    --           |  move (expr, expr)             move pen to a new position
    --           |  define macro (var*) {prog}    define a macro
    --           |  call macro (expr*)            invoke a macro
    
    -- 1. Define the abstract syntax of MiniLogo as a set of Haskell data types. 
    -- You should use built-in types for num, var, and macro. (If you want to define 
    -- a type Num, you will have to hide that name from the Prelude).
    --
    type Num = Int
    type Var = String
    type Macro = String

    type Prog = [Cmd]
    
    data Mode = Up 
              | Down
              deriving (Eq,Show)
    data Expr = Var Var
              | Num Num
              | Add Expr Expr
              deriving (Eq, Show)  
    data Cmd =  Pen Mode
              | Move Expr Expr
              | Define Macro [Var] Prog
              | Call Macro [Expr]
              deriving (Eq,Show)
    
    -- 2. Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on 
    -- the canvas) draws a line segment from (x1,y1) to (x2,y2).
    -- Write the macro in MiniLogo concrete syntax (i.e. the notation defined by the 
    -- grammar and used in the example programs above). Include this definition in a 
    -- comment in your submission.
    -- Encode the macro definition as a Haskell value using the data types defined 
    -- in Task 1. This corresponds to the abstract syntax of MiniLogo. Your Haskell 
    -- definition should start with something like line = Define "line" ...
    --
    --      Concrete syntax in a comment:
    --      line = Define line (x1,y1,x2,y2){
    --              pen up; move (x1,y1);
    --              pen down; move (x2,y2);
    --              }

    line :: Cmd
    line = Define "line" ["x1","y1","x2","y2"]
                  [Pen Up,   Move (Var "x1")(Var "y1"),
                   Pen Down, Move (Var "x2")(Var "y2")]
    
    
    
    -- Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h) 
    -- that draws a big “X” of width w and height h, starting from position (x,y). 
    -- Your definition should not contain any move commands.
    -- Write the macro in MiniLogo concrete syntax and include this definition in a comment 
    -- in your submission.
    -- Encode the macro definition as a Haskell value, representing the abstract syntax of 
    -- the definition.
    --
    --      Concrete syntax in a comment:
    --      nix = Define nix (x,y,w,h){
    --              line(x,y,x+w,y+h);
    --              line (x+w, y, x, y+h);
    --            }

    nix :: Cmd
    nix = Define "nix" ["x","y","w","h"]
                [Call "line" [Var "x", Var "y", Add (Var "x") (Var "w"), Add (Var "y") (Var "h")],
                 Call "line" [Add (Var "x")(Var "w"),Var "y", Var "x",   Add (Var "y")(Var "h")]]

    
    
    -- Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program that 
    -- draws a staircase of n steps starting from (0,0). Here is a visual illustration of 
    -- what the generated program should draw for a couple different applications of steps.

    steps :: Int -> Prog
    steps 0 = []
    steps 1 = [Pen Up, Move (Num 0)(Num 0), Pen Down, Move (Num 0)(Num 1), Move (Num 1)(Num 1), Pen Up]
    steps n = steps (n - 1) ++ [Move (Num (n - 1))(Num n), Move (Num n)(Num n)]
    
    
    -- Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names of 
    -- all of the macros that are defined anywhere in a given MiniLogo program. Don’t worry 
    -- about duplicates—if a macro is defined more than once, the resulting list may include 
    -- multiple copies of its name.


    macros :: Prog -> [Macro]
    macros [] = []
    macros ((Define macro _ _): xs) = macro: macros xs
    macros ((Pen _): xs)            = macros xs
    macros ((Call _ _): xs)         = macros xs
    macros ((Move _ _): xs)         = macros xs 
   
    
    -- Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program. 
    -- That is, it transforms the abstract syntax (a Haskell value) into nicely formatted 
    -- concrete syntax (a string of characters). Your pretty-printed program should look similar 
    -- to the example programs given above; however, for simplicity you will probably want to 
    -- print just one command per line.
    -- In GHCi, you can render a string with newlines by applying the function putStrLn. 
    -- So, to pretty-print a program p use: putStrLn (pretty p).

    pretty :: Prog -> String
    pretty []= ""
    pretty ((Pen pen): xs ) = "Pen " ++ (case pen of
                                    Up -> "Up; "
                                    Down -> "Down; ") ++ pretty xs
    pretty ((Move l r) : xs) = "Move(" ++ prettyExpr l ++ "," ++ prettyExpr r ++ "); " ++ pretty xs
    pretty ((Define name args prog) : xs) = "define " ++ show name ++ " (" ++ prettyArgs args ", " 
                                    ++ ") {" ++ pretty prog ++ "}" ++ pretty xs
    pretty ((Call name args) : xs) = "Call (" ++ prettyArgs (map prettyExpr args) ", " 
                                    ++ "); " ++ pretty xs


    -- This is just a helper function that turns expressions into strings
    -- source: https://stackoverflow.com/questions/2784271/haskell-converting-int-to-string
    prettyExpr :: Expr -> String
    prettyExpr (Var x) = x 
    prettyExpr (Num x) = show x
    prettyExpr (Add l r) = prettyExpr l ++ " + " ++ prettyExpr r 


    -- This is a helper function that will print lists
    prettyArgs :: [String] -> String ->[Char]
    prettyArgs [] _ = ""
    prettyArgs (x:xs) s = x ++ s ++ prettyArgs xs s
    

    
    
    
    -- Bonus Problems

    -- 7. Define a Haskell function optE :: Expr -> Expr that partially evaluates expressions by 
    -- replacing any additions of literals with the result. For example, given the expression 
    -- (2+3)+x, optE should return the expression 5+x.

    optE :: Expr -> Expr
    optE (Var x) = Var x
    optE (Num x) = Num x
    optE (Add x y) = Add (optE x) (optE y)
    
    
    -- 8. Define a Haskell function optP :: Prog -> Prog that optimizes all of the expressions 
    -- contained in a given program using optE.
    optP :: Prog -> Prog
    optP (x:xs) = optProg x : optP xs

    optProg :: Cmd -> Cmd
    optProg (Call x y) = Call x (map optE y)
    optProg (Move x y) = Move (optE x) (optE y)
    optProg (Pen x) = (Pen x)