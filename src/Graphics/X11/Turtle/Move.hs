module Graphics.X11.Turtle.Move (
	Field,
	Layer,
	Character,

	forkIOX,
	openField,
	closeField,
	addLayer,
	addCharacter,
	fieldSize,
	clearLayer,
	clearCharacter,
	addThread,
	fieldColor,
	onclick,
	onrelease,
	ondrag,
	onkeypress,
	waitField,
	writeString,
--	Color'(..),

	moveTurtle
) where

import Graphics.X11.Turtle.State(TurtleState(..))
import Graphics.X11.Turtle.Field(
	withLock2,
	Field, Layer, Character,
	forkIOX, openField, closeField, flushLayer,
	addLayer, addCharacter, fieldSize, clearLayer,
	drawCharacter, drawCharacterAndLine, undoLayer,
	drawLine,
	clearCharacter, addThread,
	fieldColor, onkeypress, onclick, onrelease, ondrag, waitField, writeString
 )
import Text.XML.YJSVG

import Control.Concurrent(threadDelay)
import Control.Monad(when, unless, forM_)
import Control.Arrow((***))
import Data.Maybe

type Pos = (Double, Double)

step :: Double
step = 10

moveSpeed :: Int
moveSpeed = 50000

stepDir :: Double
stepDir = 1 / 72

rotateSpeed :: Int
rotateSpeed = 10000

dir :: TurtleState -> Double
dir t = direction t / degrees t

moveTurtle :: Field -> Character -> Layer -> TurtleState -> TurtleState -> IO ()
moveTurtle f c l t0 t1 = flip withLock2 f $ const $ do
	when (undo t1 && isJust (draw t0)) $ do
		done <- undoLayer l
		unless done $ clearLayer l >> drawLines f l (drawed t1)
	when (undo t1 && clear t0) $ drawLines f l $ drawed t1
	when (visible t1) $ do
		forM_ (getDirections (dir t0) (dir t1)) $ \d -> do
			drawTurtle f c (pencolor t1) (shape t1) (shapesize t1) d
				(pensize t1) p0 Nothing
			flushLayer f
			threadDelay rotateSpeed
		forM_ (getPositions x0 y0 x1 y1) $ \p -> do
			drawTurtle f c (pencolor t1) (shape t1) (shapesize t1)
				(dir t1) (pensize t1) p lineOrigin
			flushLayer f
			threadDelay moveSpeed
		drawTurtle f c (pencolor t1) (shape t1) (shapesize t1) (dir t1)
			(pensize t1) p1 lineOrigin
	unless (visible t1) $ clearCharacter f c
	when (clear t1) $ clearLayer l -- >> flushLayer f
	unless (undo t1) $ drawDraw f l (draw t1) -- >> flushLayer f
	flushLayer f
	where
	(tl, to) = if undo t1 then (t0, t1) else (t1, t0)
	lineOrigin = if pendown tl then Just $ position to else Nothing
	p0@(x0, y0) = position t0
	p1@(x1, y1) = position t1

drawLines :: Field -> Layer -> [SVG] -> IO ()
drawLines f l = mapM_ (drawDraw f l . Just) . reverse

drawDraw :: Field -> Layer -> Maybe SVG -> IO ()
drawDraw _ _ Nothing = return ()
drawDraw f l (Just (Line (Center x0 y0) (Center x1 y1) clr lw)) =
	drawLine f l lw clr x0 y0 x1 y1
-- drawDraw l (Line clr lw (x0, y0) (x1, y1)) = drawLine l lw (clr) x0 y0 x1 y1
drawDraw f l (Just (Text (Center x y) sz clr fnt str)) =
-- drawDraw l (Just (Text clr fnt sz (x, y) str)) =
	writeString f l fnt sz clr x y str
{-
	where
	[r, g, b] = map ((/ 0xff) . fromIntegral) [r_, g_, b_]
-}
drawDraw _ _ _ = error "not implemented"

getPositions :: Double -> Double -> Double -> Double -> [Pos]
getPositions x0 y0 x1 y1 = take num $ zip [x0, x0 + dx .. ] [y0, y0 + dy .. ]
	where
	num = floor $ dist / step
	dist = ((x1 - x0) ** 2 + (y1 - y0) ** 2) ** (1/2)
	dx = step * (x1 - x0) / dist
	dy = step * (y1 - y0) / dist

getDirections :: Double -> Double -> [Double]
getDirections ds de = [ds, ds + dd .. de - dd]
	where
	dd = if de > ds then stepDir else - stepDir

drawTurtle :: Field -> Character -> Color -> [Pos] -> Double -> Double -> Double ->
	Pos -> Maybe Pos -> IO ()
drawTurtle f c clr sh s d lw (px, py) org = do
	let sp = map (((+ px) *** (+ py)) . rotatePoint . ((* s) *** (* s))) sh
	maybe (drawCharacter f c clr sp)
		(\(x0, y0) -> (drawCharacterAndLine f c clr sp lw x0 y0 px py)) org
--	flushLayer f
	where
	rotatePoint (x, y) = let rad = d * 2 * pi in
		(x * cos rad - y * sin rad, x * sin rad + y * cos rad)