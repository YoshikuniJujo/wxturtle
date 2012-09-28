import Graphics.UI.WX.Turtle
import Graphics.UI.WX

main = start gui

gui = do
	f <- openField
	t <- newTurtle f
	oninputtext f $ action t
	speed t "slowest"
--	left t 45
--	forward t 300
--	right t 135
--	forward t 250
--	undo t
--	ff <- frame [text := "Hello!"]
--	quit <- button ff [text := "Quit", on command := close ff]
--	set ff [layout := widget quit]

action t "quit" = return False
action t "forward" = forward t 100 >> return True
action t "backward" = backward t 100 >> return True
action t "undo" = undo t >> return True
action t "left" = left t 90 >> return True
action t "right" = right t 90 >> return True
action t "red" = pencolor t (255, 0, 0) >> return True
action t "blue" = fillcolor t (0, 0, 255) >> return True
action t "bold" = pensize t 5 >> return True
action t "normal" = pensize t 1 >> return True
action t "big" = shapesize t 3 3 >> return True
action t "turtle" = shape t "turtle" >> return True
action t "stamp" = stamp t >> return True
action t "penup" = penup t >> return True
action t "begin" = beginfill t >> return True
action t "end" = endfill t >> return True
action t "turtlecolor" = pencolor t (0, 0, 255) >> fillcolor t (0, 255, 0) >> return True
action _ _ = return True
