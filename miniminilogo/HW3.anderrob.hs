-- Robert Hayden Anderson
-- anderrob
-- anderrob@oregonstate.edu
-- cs381
-- summer 2018

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
    amazing = (bigbox 20 0) ++ (doBotLeft) ++ (doTopLeft)  ++ (doTopRight) ++ (doBotRight) ++ doM ++ doE ++ doH
    -- amazing = (bigbox 20 0) ++ (doTheThing 40 0 20 5)
    
    bigbox :: Int -> Int -> Prog
    bigbox x y = [Pen Up, Move x y, Pen Down,
        Move (x+40) y, Move (x+40) (y+40), Move x (y+40), Move x y]
    

    -- doMeh :: Prog
    -- doMeh = doM ++ doE ++ doH
    
    doM :: Prog
    doM     =   (doLine (29,15)(29,25))
            ++  (doLine (29,25)(32,20))
            ++  (doLine (32,20)(35,25))
            ++  (doLine (35,25)(35,15))
    
    doE :: Prog
    doE     =   (doLine (38,25)(38,15))
            ++  (doLine (38,15)(43,15))
            ++  (doLine (38,25)(43,25))
            ++  (doLine (38,20)(42,20))
    doH :: Prog
    doH     =   (doLine (45,25)(45,15))
            ++  (doLine (52,25)(52,15))
            ++  (doLine (45,20)(52,20))



    
    doLine :: Point -> Point -> Prog
    doLine (x,y) (x2,y2) = [Pen Up, Move x y, Pen Down, Move x2 y2]
    doBotLeft :: Prog
    doBotLeft =    (doLine ( 40 , 0 ) ( 20 , 00 )) 
                ++ (doLine ( 39 , 0 ) ( 20 , 01 )) 
                ++ (doLine ( 38 , 0 ) ( 20 , 02 ))
                ++ (doLine ( 37 , 0 ) ( 20 , 03 ))
                ++ (doLine ( 36 , 0 ) ( 20 , 04 ))
                ++ (doLine ( 35 , 0 ) ( 20 , 05 )) 
                ++ (doLine ( 34 , 0 ) ( 20 , 06 )) 
                ++ (doLine ( 33 , 0 ) ( 20 , 07 ))
                ++ (doLine ( 32 , 0 ) ( 20 , 08 ))
                ++ (doLine ( 31 , 0 ) ( 20 , 09 ))
                ++ (doLine ( 30 , 0 ) ( 20 , 10 )) 
                ++ (doLine ( 29 , 0 ) ( 20 , 11 ))
                ++ (doLine ( 28 , 0 ) ( 20 , 12 ))
                ++ (doLine ( 27 , 0 ) ( 20 , 13 ))
                ++ (doLine ( 26 , 0 ) ( 20 , 14 )) 
                ++ (doLine ( 25 , 0 ) ( 20 , 15 ))
                ++ (doLine ( 24 , 0 ) ( 20 , 16 ))
                ++ (doLine ( 23 , 0 ) ( 20 , 17 ))
                ++ (doLine ( 22 , 0 ) ( 20 , 18 ))
                ++ (doLine ( 21 , 0 ) ( 20 , 19 ))
                ++ (doLine ( 20 , 0 ) ( 20 , 20 ))
    doTopLeft :: Prog
    doTopLeft =     (doLine ( 20 , 20 ) ( 20 , 40 )) 
                ++  (doLine ( 20 , 21 ) ( 21 , 40 )) 
                ++  (doLine ( 20 , 22 ) ( 22 , 40 ))
                ++  (doLine ( 20 , 23 ) ( 23 , 40 ))
                ++  (doLine ( 20 , 24 ) ( 24 , 40 ))
                ++  (doLine ( 20 , 25 ) ( 25 , 40 )) 
                ++  (doLine ( 20 , 26 ) ( 26 , 40 ))
                ++  (doLine ( 20 , 27 ) ( 27 , 40 ))
                ++  (doLine ( 20 , 28 ) ( 28 , 40 ))
                ++  (doLine ( 20 , 29 ) ( 29 , 40 )) 
                ++  (doLine ( 20 , 30 ) ( 30 , 40 ))
                ++  (doLine ( 20 , 31 ) ( 31 , 40 ))
                ++  (doLine ( 20 , 32 ) ( 32 , 40 ))
                ++  (doLine ( 20 , 33 ) ( 33 , 40 )) 
                ++  (doLine ( 20 , 34 ) ( 34 , 40 ))
                ++  (doLine ( 20 , 35 ) ( 35 , 40 ))
                ++  (doLine ( 20 , 36 ) ( 36 , 40 ))
                ++  (doLine ( 20 , 37 ) ( 37 , 40 )) 
                ++  (doLine ( 20 , 38 ) ( 38 , 40 ))
                ++  (doLine ( 20 , 39 ) ( 39 , 40 ))
                ++  (doLine ( 20 , 40 ) ( 40 , 40 ))

    doTopRight :: Prog
    doTopRight=     (doLine ( 60 , 20 ) ( 60 , 40 )) 
                ++  (doLine ( 60 , 21 ) ( 59 , 40 )) 
                ++  (doLine ( 60 , 22 ) ( 58 , 40 ))
                ++  (doLine ( 60 , 23 ) ( 57 , 40 ))
                ++  (doLine ( 60 , 24 ) ( 56 , 40 ))
                ++  (doLine ( 60 , 25 ) ( 55 , 40 )) 
                ++  (doLine ( 60 , 26 ) ( 54 , 40 ))
                ++  (doLine ( 60 , 27 ) ( 53 , 40 ))
                ++  (doLine ( 60 , 28 ) ( 52 , 40 ))
                ++  (doLine ( 60 , 29 ) ( 51 , 40 )) 
                ++  (doLine ( 60 , 30 ) ( 50 , 40 ))
                ++  (doLine ( 60 , 31 ) ( 49 , 40 ))
                ++  (doLine ( 60 , 32 ) ( 48 , 40 ))
                ++  (doLine ( 60 , 33 ) ( 47 , 40 )) 
                ++  (doLine ( 60 , 34 ) ( 46 , 40 ))
                ++  (doLine ( 60 , 35 ) ( 45 , 40 ))
                ++  (doLine ( 60 , 36 ) ( 44 , 40 ))
                ++  (doLine ( 60 , 37 ) ( 43 , 40 )) 
                ++  (doLine ( 60 , 38 ) ( 42 , 40 ))
                ++  (doLine ( 60 , 39 ) ( 41 , 40 ))
                ++  (doLine ( 60 , 40 ) ( 40 , 40 ))
                
    doBotRight :: Prog
    doBotRight =   (doLine ( 40 , 0 ) ( 60 , 00 )) 
                ++ (doLine ( 41 , 0 ) ( 60 , 01 )) 
                ++ (doLine ( 42 , 0 ) ( 60 , 02 ))
                ++ (doLine ( 43 , 0 ) ( 60 , 03 ))
                ++ (doLine ( 44 , 0 ) ( 60 , 04 ))
                ++ (doLine ( 45 , 0 ) ( 60 , 05 )) 
                ++ (doLine ( 46 , 0 ) ( 60 , 06 ))
                ++ (doLine ( 47 , 0 ) ( 60 , 07 ))
                ++ (doLine ( 48 , 0 ) ( 60 , 08 ))
                ++ (doLine ( 49 , 0 ) ( 60 , 09 )) 
                ++ (doLine ( 50 , 0 ) ( 60 , 10 ))
                ++ (doLine ( 51 , 0 ) ( 60 , 11 ))
                ++ (doLine ( 52 , 0 ) ( 60 , 12 ))
                ++ (doLine ( 53 , 0 ) ( 60 , 13 )) 
                ++ (doLine ( 54 , 0 ) ( 60 , 14 ))
                ++ (doLine ( 55 , 0 ) ( 60 , 15 ))
                ++ (doLine ( 56 , 0 ) ( 60 , 16 ))
                ++ (doLine ( 57 , 0 ) ( 60 , 17 )) 
                ++ (doLine ( 58 , 0 ) ( 60 , 18 ))
                ++ (doLine ( 59 , 0 ) ( 60 , 19 ))
                ++ (doLine ( 60 , 0 ) ( 60 , 20 ))  
                
                
                