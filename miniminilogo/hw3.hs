module HW3 where

    import MiniMiniLogo
    import Render
    
    
    --
    -- * Semantics of MiniMiniLogo
    --
    
    -- NOTE:
    --   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
    --     functions for generating MiniMiniLogo programs. It contains the type
    --     definitions for Mode, Cmd, and Prog.
    --   * Render.hs contains code for rendering the output of a MiniMiniLogo
    --     program in HTML5. It contains the types definitions for Point and Line.
    
    -- | A type to represent the current state of the pen.
    type State = (Mode,Point)
    
    -- | The initial state of the pen.
    start :: State
    start = (Up,(0,0))
    
    -- | A function that renders the image to HTML. Only works after you have
    --   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
    --   produce an HTML file named MiniMiniLogo.html, which you can load in
    --   your browswer to view the rendered image.
    draw :: Prog -> IO ()
    draw p = let (_,ls) = prog p start in toHTML ls
    
    
    -- Semantic domains:
    --   * Cmd:  State -> (State, Maybe Line)
    --   * Prog: State -> (State, [Line])
    
    
    -- | Semantic function for Cmd.
    --   
    --   >>> cmd (Pen Down) (Up,(2,3))
    --   ((Down,(2,3)),Nothing)
    --
    --   >>> cmd (Pen Up) (Down,(2,3))
    --   ((Up,(2,3)),Nothing)
    --
    --   >>> cmd (Move 4 5) (Up,(2,3))
    --   ((Up,(4,5)),Nothing)
    --
    --   >>> cmd (Move 4 5) (Down,(2,3))
    --   ((Down,(4,5)),Just ((2,3),(4,5)))
    --
    cmd :: Cmd -> State -> (State, Maybe Line)
    cmd (Pen x) (_,y) = ((x,y), (Nothing)) 
    cmd (Move x y) (Down, z) = ((Down, (x, y)), Just ((z), (x,y)))
    cmd (Move x y) (Up, z) = ((Up, (x, y)), Nothing)
    
    
    -- | Semantic function for Prog.
    --
    --   >>> prog (nix 10 10 5 7) start
    --   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
    --
    --   >>> prog (steps 2 0 0) start
    --   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
    prog :: Prog -> State -> (State, [Line])
    prog program st = helpProg program (st, []) 
    


    helpProg :: Prog -> (State, [Line]) -> (State, [Line])
    helpProg [] st = st
    helpProg (x:xs) (st , ls) =   
                    let (sta, lin) = cmd x st in
                    case lin of
                        Just lin -> helpProg xs (sta, ls ++ [lin])
                        Nothing -> helpProg xs (sta, ls)
                                  
    
    --
    -- * Extra credit
    --
    
    -- | This should be a MiniMiniLogo program that draws an amazing picture.
    --   Add as many helper functions as you want.
    amazing :: Prog
    amazing = [Pen Up, Move 5 5,Pen Down, Move 20 5, Move 20 10, Move 5 10, Move 5 5, Pen Up] ++ (steps 5 20 10)
        ++ [Pen Up, Move 5 10, Pen Down, Move 3 13, Pen Up] ++ (bigbox 0 13) 
        ++ [Pen Up, Move 0 17, Pen Down, Move 1 19,Move 2 17, Move 3 19, Move 4 17, Pen Up]
        ++ [Pen Up, Move 7 5, Pen Down, Move 7 0, Pen Up]
        ++ [Pen Up, Move 17 5, Pen Down, Move 17 0, Pen Up]

    bigbox :: Int -> Int -> Prog
    bigbox x y = [Pen Up, Move x y, Pen Down,
        Move (x+4) y, Move (x+4) (y+4), Move x (y+4), Move x y]