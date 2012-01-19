module Turtle (
	openField,
	newTurtle,
	shape,
	shapesize,
) where

import CharAndBG
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
	putStrLn "module Turtle"

initForTest :: IO Turtle
initForTest = do
	f <- openField
	threadDelay 1000000
	t <- newTurtle f
	shape t "turtle"
	shapesize t 3
	return t

forward, backward :: Turtle -> Double -> IO ()
forward t dist = do
	setUndoN t 1
	forwardNotSetUndo t dist

forwardNotSetUndo t dist = do
	dir <- direction t
	(x0, y0) <- position t
	let	xd = dist * cos (dir * pi / 180)
		yd = dist * sin (dir * pi / 180)
	goto t (x0 + xd) (y0 + yd)


backward t = forward t . negate

left, right :: Turtle -> Double -> IO ()
left t dd = do
--	setUndoN t 0
	leftNotSetUndo t dd
leftNotSetUndo t dd = do
	dir <- direction t
	rotate t (dir + dd)

right t = left t . negate

circle :: Turtle -> Double -> IO ()
circle t r = do
	setUndoN t 72
	replicateM_ 36 $ forwardNotSetUndo t (2 * r * pi / 36) >>
		leftNotSetUndo t 10
