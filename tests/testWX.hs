import Graphics.UI.WX.Turtle
import Graphics.UI.WX

main = start gui

gui = do
	f <- openField
	t <- newTurtle f
	speed t "slowest"
	left t 45
	forward t 300
	right t 135
	forward t 250
	undo t
--	ff <- frame [text := "Hello!"]
--	quit <- button ff [text := "Quit", on command := close ff]
--	set ff [layout := widget quit]
