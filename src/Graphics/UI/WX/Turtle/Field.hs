{-# LANGUAGE DoRec #-}

module Graphics.UI.WX.Turtle.Field(
	-- * types and classes
	Field(fFrame),
	Layer,
	Character,
	Coordinates(..),

	-- * basic functions
	openField,
	closeField,
	waitField,
	topleft,
	center,
	coordinates,
	fieldSize,

	-- * draw
	forkField,
	flushField,
	fieldColor,

	-- ** to Layer
	addLayer,
	drawLine,
	fillRectangle,
	fillPolygon,
	writeString,
	drawImage,
	undoLayer,
	undoField,
	clearLayer,

	-- ** to Character
	addCharacter,
	drawCharacter,
	drawCharacterAndLine,
	clearCharacter,

	-- * event driven
	oninputtext,
	onclick,
	onrelease,
	ondrag,
	onmotion,
	onkeypress,
	ontimer
) where

import Graphics.UI.WX(
	on, command, Prop(..), text, button, frame, layout, widget, set, panel,
	Frame, Panel, minsize, sz, column, circle, paint, Point2(..), Point,
	line, repaint, DC, Rect, dcClear, polygon, red, green, brushColor, penColor,
	rgb, row, textEntry, processEnter, get, hfill, fill, size, Size2D(..), resize,
	TextCtrl, penWidth
 )
import qualified Graphics.UI.WX as WX

import Graphics.UI.WX.Turtle.Layers(
	Layers, Layer, Character, newLayers, redrawLayers,
	makeLayer, background, addDraw, undoLayer, clearLayer,
	makeCharacter, character)
import Text.XML.YJSVG(Position(..), Color(..))

import Control.Monad(when, unless, forever, replicateM, forM_, join)
import Control.Monad.Tools(doWhile_, doWhile)
import Control.Arrow((***))
import Control.Concurrent(
	ThreadId, forkIO, killThread, threadDelay,
	Chan, newChan, readChan, writeChan)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import Data.IORef.Tools(atomicModifyIORef_)
import Data.Maybe(fromMaybe)
import Data.List(delete)
import Data.Convertible(convert)
import Data.Function.Tools(const2, const3)

--------------------------------------------------------------------------------

data Coordinates = CoordTopLeft | CoordCenter

data Field = Field{
	fFrame :: Frame (),
	fPanel :: Panel (),
	fAction :: IORef (DC () -> Rect -> IO ()),
	fActions :: IORef [DC () -> Rect -> IO ()],
	fCharacter :: Character,
	fCoordinates :: Coordinates,

	fPenColor :: (Int, Int, Int),
	fDrawColor :: (Int, Int, Int),

	fSize :: IORef (Double, Double),

	fInputtext :: IORef (String -> IO Bool),

	fLayers :: IORef Layers
 }

--------------------------------------------------------------------------------

undoField :: Field -> IO ()
undoField f = do
	atomicModifyIORef_ (fActions f) tail

openField :: IO Field
openField = do
	fr <- frame [text := "turtle"]
	p <- panel fr []
	quit <- button fr [text := "Quit", on command := WX.close fr]
	inputAction <- newIORef $ \_ -> return True
	rec input <- textEntry fr [
		processEnter := True,
		on command := do
			str <- get input text
			putStrLn str
			set input [text := ""]
			cont <- ($ str) =<< readIORef inputAction
			when (not cont) $ WX.close fr]
	set fr [layout := column 5 [
		fill $ minsize (sz 300 300) $ widget p,
		row 5 [hfill $ widget input] ]] -- , widget quit] ]]
	act <- newIORef $ \dc rct -> circle dc (Point 40 25) 25 []
	acts <- newIORef []
	layers <- newLayers 50 (return ())
--		(writeIORef act (\dc rect -> dcClear dc) >> repaint p)
		(return ())
		(return ())
--		(writeIORef act (\dc rect -> dcClear dc) >> repaint p)
--		(return ())
	Size x y <- get p size
	print x
	print y
	sz <- newIORef (fromIntegral x, fromIntegral y)
	let	f = Field{
			fFrame = fr,
			fPanel = p,
			fAction = act,
			fActions = acts,
			fCoordinates = CoordCenter,
			fPenColor = (0, 0, 0),
			fSize = sz,
			fLayers = layers,
			fInputtext = inputAction
		 }
	set p [	on paint := \dc rct -> do
			act <- readIORef $ fAction f
			acts <- readIORef $ fActions f
			mapM_ (($ rct) . ($ dc)) acts
			act dc rct,
		on resize := do
			Size x y <- get p size
			writeIORef sz (fromIntegral x, fromIntegral y) ]
	return f

data InputType = XInput | End | Timer

waitInput :: Field -> IO (Chan ())
waitInput f = newChan

closeField :: Field -> IO ()
closeField = WX.close . fFrame

waitField :: Field -> IO ()
waitField = const $ return ()

topleft, center :: Field -> IO ()
topleft = const $ return ()
center = const $ return ()

coordinates :: Field -> IO Coordinates
coordinates = return . fCoordinates

fieldSize :: Field -> IO (Double, Double)
fieldSize = const $ return (0, 0)

--------------------------------------------------------------------------------

forkField :: Field -> IO () -> IO ThreadId
forkField f act = do
	tid <- forkIO act
	return tid

flushField :: Field -> Bool -> IO a -> IO a
flushField f real act = act

fieldColor :: Field -> Layer -> Color -> IO ()
fieldColor f l clr = return ()

--------------------------------------------------------------------------------

addLayer = makeLayer . fLayers

drawLayer f l drw = addDraw l (drw, drw)

drawLine :: Field -> Layer -> Double -> Color -> Position -> Position -> IO ()
drawLine f l w c p q = do
	atomicModifyIORef_ (fActions f) $ (act :)
	repaint $ fPanel f
	where
	act = \dc rect -> do 
		set dc [penColor := colorToWX c, penWidth := round w]
		p' <- positionToPoint f p
		q' <- positionToPoint f q
		line dc p' q' []

getPenColor :: Field -> WX.Color
getPenColor Field{fPenColor = (r, g, b)} = rgb r g b

colorToWX :: Color -> WX.Color
colorToWX (RGB{colorRed = r, colorGreen = g, colorBlue = b}) = rgb r g b

{- addDraw l (do
	atomicModifyIORef_ (fAction f) $ \f dc rect -> do
		f dc rect
		line dc (positionToPoint p) (positionToPoint q) []
	repaint $ fPanel f, do
	atomicModifyIORef_ (fAction f) $ \f dc rect -> do
		f dc rect
		line dc (positionToPoint p) (positionToPoint q) []
	repaint $ fPanel f)
-}

positionToPoint :: Field -> Position -> IO Point
positionToPoint f (Center x y) = do
	(sx, sy) <- readIORef $ fSize f
	return $ Point (round $ x + sx / 2) (round $ - y + sy / 2)

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f l fname size clr pos str = return ()

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage f l fp pos w h = return ()

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f l p w h clr = return ()

fillPolygon :: Field -> Layer -> [Position] -> Color -> Color -> Double -> IO ()
fillPolygon f l ps clr lc lw = do
	atomicModifyIORef_ (fActions f) $ (act :)
	repaint $ fPanel f
	where
	act = \dc rect -> do
		set dc [brushColor := colorToWX clr, penColor := colorToWX lc,
			penWidth := round lw]
		sh' <- mapM (positionToPoint f) ps
		polygon dc sh' []

--------------------------------------------------------------------------------

addCharacter = makeCharacter . fLayers

drawCharacter :: Field -> Character -> Color -> Color -> [Position] -> Double -> IO ()
drawCharacter f ch fc c ps lw = do
	writeIORef (fAction f) $ \dc rect -> do
		set dc [brushColor := colorToWX fc, penColor := colorToWX c,
			penWidth := round lw]
		sh' <- mapM (positionToPoint f) ps
		polygon dc sh' []
	repaint $ fPanel f

drawCharacterAndLine ::	Field -> Character -> Color -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f ch fclr clr sh lw p q = do
--	putStrLn $ "drawCharacterAndLine" ++ show p ++ " : " ++ show q
	writeIORef (fAction f) $ \dc rect -> do
		set dc [brushColor := colorToWX fclr, penColor := colorToWX clr,
			penWidth := round lw]
		p' <- positionToPoint f p
		q' <- positionToPoint f q
		line dc p' q' []
		sh' <- mapM (positionToPoint f) sh
		polygon dc sh' []
	repaint $ fPanel f
		
{-
	atomicModifyIORef_ (fAction f) $ \f dc rect -> do
		f dc rect
		line dc (positionToPoint p) (positionToPoint q) []
-}
	repaint $ fPanel f

clearCharacter :: Character -> IO ()
clearCharacter ch = character ch $ return ()

--------------------------------------------------------------------------------

oninputtext :: Field -> (String -> IO Bool) -> IO ()
oninputtext = writeIORef . fInputtext

onclick, onrelease :: Field -> (Int -> Double -> Double -> IO Bool) -> IO ()
onclick _ _ = return ()
onrelease _ _ = return ()

ondrag :: Field -> (Int -> Double -> Double -> IO ()) -> IO ()
ondrag _ _ = return ()

onmotion :: Field -> (Double -> Double -> IO ()) -> IO ()
onmotion _ _ = return ()

onkeypress :: Field -> (Char -> IO Bool) -> IO ()
onkeypress _ _ = return ()

ontimer :: Field -> Int -> IO Bool -> IO ()
ontimer f t fun = return ()
