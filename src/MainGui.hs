module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import NTree
import FSTree
import ParserHtml

main :: IO()
main = start gui

gui :: IO()
gui = do f   <- frame [text := "gui"]
         inp <- entry  f [text := "./test/test1.html"]
         pnl <- panel f [on paint := draw inp] --[bgcolor := white]
         go  <- button f [text := "Paint"]
         set go [on command := repaint pnl]
         set f [layout := column 5 [row 5 [hfill $ widget inp, widget go], fill $ widget pnl]]
         return ()

draw inp dc rt = do
    file   <- get inp text
    ast    <- parseHtml file
    let fstree = sem_Root (Root ast)
    let res    = sem_BoxRoot (BoxRoot fstree)
    --print res
    mapM_ (drect dc) res
    where drect dc (Block x y bl)  = drawRect dc (rect (pt x y) (sz 80 50)) (if bl then [brush := BrushStyle BrushSolid yellow] else [])
          drect dc (Name str x y)  = drawText dc str (pt x y) []
          drect dc (Line (x1,y1) (x2,y2))
                                   = line dc (pt x1 y1) (pt x2 y2) []

