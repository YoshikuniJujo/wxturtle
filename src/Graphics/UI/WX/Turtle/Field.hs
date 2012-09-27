module Graphics.UI.WX.Turtle.Field(
	-- * types and classes
	Field,
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
	line, repaint, DC, Rect, dcClear
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
	set fr [layout := column 5 [widget quit, minsize (sz 300 300) $ widget p ]]
	act <- newIORef $ \dc rct -> circle dc (Point 40 25) 25 []
	acts <- newIORef []
	layers <- newLayers 50 (return ())
--		(writeIORef act (\dc rect -> dcClear dc) >> repaint p)
		(return ())
		(return ())
--		(writeIORef act (\dc rect -> dcClear dc) >> repaint p)
--		(return ())
	let	f = Field{
			fFrame = fr,
			fPanel = p,
			fAction = act,
			fActions = acts,
			fCoordinates = CoordCenter,
			fLayers = layers
		 }
	set p [on paint := \dc rct -> do
		act <- readIORef $ fAction f
		acts <- readIORef $ fActions f
		mapM_ (($ rct) . ($ dc)) acts
		act dc rct]
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
		line dc (positionToPoint p) (positionToPoint q) []

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

positionToPoint :: Position -> Point
positionToPoint (Center x y) = Point (round x) (round y)

writeString :: Field -> Layer -> String -> Double -> Color -> Position ->
	String -> IO ()
writeString f l fname size clr pos str = return ()

drawImage :: Field -> Layer -> FilePath -> Position -> Double -> Double -> IO ()
drawImage f l fp pos w h = return ()

fillRectangle :: Field -> Layer -> Position -> Double -> Double -> Color -> IO ()
fillRectangle f l p w h clr = return ()

fillPolygon :: Field -> Layer -> [Position] -> Color -> IO ()
fillPolygon f l ps clr = return ()

--------------------------------------------------------------------------------

addCharacter = makeCharacter . fLayers

drawCharacter :: Field -> Character -> Color -> [Position] -> IO ()
drawCharacter f ch c ps = do
	putStrLn "drawCharacter"

drawCharacterAndLine ::	Field -> Character -> Color -> [Position] ->
	Double -> Position -> Position -> IO ()
drawCharacterAndLine f ch clr sh lw p q = do
	putStrLn $ "drawCharacterAndLine" ++ show p ++ " : " ++ show q
	writeIORef (fAction f) $ \dc rect -> do
		line dc (positionToPoint p) (positionToPoint q) []
		
{-
	atomicModifyIORef_ (fAction f) $ \f dc rect -> do
		f dc rect
		line dc (positionToPoint p) (positionToPoint q) []
-}
	repaint $ fPanel f

clearCharacter :: Character -> IO ()
clearCharacter ch = character ch $ return ()

--------------------------------------------------------------------------------

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
