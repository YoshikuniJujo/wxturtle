module Graphics.X11.WindowLayers(
	Field,
	Layer,
	Character,

	openField,
	closeField,
	layerSize,

	addLayer,
	addCharacter,

	drawLine,
	drawCharacter,
	drawCharacterAndLine,

	undoLayer,
	clearLayer,

	forkIOX
) where

import Graphics.X11(
	Display, Window, Pixmap, Atom, GC, Point(..), Dimension, Position,

	openDisplay, closeDisplay, flush, defaultScreen, rootWindow,
	whitePixel, blackPixel,	defaultDepth,
	createSimpleWindow, mapWindow, createPixmap, internAtom, createGC,

	setForeground, copyArea,
	fillRectangle, fillPolygon, nonconvex, coordModeOrigin,

	setWMProtocols, selectInput, allocaXEvent, nextEvent,
	keyPressMask, exposureMask,

	getGeometry, initThreads
 )
import qualified Graphics.X11 as X (drawLine)
import Graphics.X11.Xlib.Extras(Event(..), getEvent)

import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Bits((.|.))
import Data.Convertible(convert)
import Data.List.Tools(modifyAt, setAt)
import Data.Bool.Tools(whether)

import Control.Monad(replicateM, forM_)
import Control.Monad.Tools(doWhile_)
import Control.Arrow((***))
import Control.Concurrent(forkIO, ThreadId)

data Field = Field{
	fDisplay :: Display,
	fWindow :: Window,
	fGC :: GC,
	fGCBG :: GC,
	fDel :: Atom,
	fUndoBuf :: Pixmap,
	fBG :: Pixmap,
	fBuf :: Pixmap,
	fWidth :: IORef Dimension,
	fHeight :: IORef Dimension,
	fBuffed :: IORef [IO ()],
	fLayers :: IORef [[Bool -> IO ()]],
	fCharacters :: IORef [IO ()]
 }

data Layer = Layer{
	layerField :: Field,
	layerId :: Int
 }

data Character = Character{
	characterField :: Field,
	characterId :: Int
 }

forkIOX :: IO () -> IO ThreadId
forkIOX = (initThreads >>) . forkIO

openField :: IO Field
openField = do
	_ <- initThreads
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(_, _, _, rWidth, rHeight, _, _) <- getGeometry dpy root
	let	black = blackPixel dpy scr
		white = whitePixel dpy scr
		depth = defaultDepth dpy scr
	bufs <- replicateM 3 $ createPixmap dpy root rWidth rHeight depth
	win <- createSimpleWindow dpy root 0 0 rWidth rHeight 1 black white
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \bf -> fillRectangle dpy bf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	[widthRef, heightRef] <- mapM newIORef [rWidth, rHeight]
	buffActions <- newIORef []
	layerActions <- newIORef []
	characterActions <- newIORef []
	let f = Field{
		fDisplay = dpy,
		fWindow = win,
		fGC = gc,
		fGCBG = gcBG,
		fDel = del,
		fUndoBuf = head bufs,
		fBG = bufs !! 1,
		fBuf = bufs !! 2,
		fWidth = widthRef,
		fHeight = heightRef,
		fBuffed = buffActions,
		fLayers = layerActions,
		fCharacters = characterActions
	 }
	_ <- forkIOX $ runLoop f
	flushWin f
	return f

runLoop :: Field -> IO ()
runLoop f = (>> closeField f) $	doWhile_ $ allocaXEvent $ \e -> do
	nextEvent (fDisplay f) e
	ev <- getEvent e
	case ev of
		ExposeEvent{} -> do
			(_, _, _, width, height, _, _) <-
				getGeometry (fDisplay f) (fWindow f)
			writeIORef (fWidth f) width
			writeIORef (fHeight f) height
			redrawAll f
			return True
		KeyEvent{} -> return True
		ClientMessageEvent{} ->
			return $ convert (head $ ev_data ev) /= fDel f
		_ -> return True

closeField :: Field -> IO ()
closeField = closeDisplay . fDisplay

layerSize :: Layer -> IO (Double, Double)
layerSize = fieldSize . layerField

fieldSize :: Field -> IO (Double, Double)
fieldSize w = fmap (fromIntegral *** fromIntegral) $ fieldSizeRaw w

fieldSizeRaw :: Field -> IO (Dimension, Dimension)
fieldSizeRaw w = do
	width <- readIORef $ fWidth w
	height <- readIORef $ fHeight w
	return (width, height)

addLayer :: Field -> IO Layer
addLayer f = do
	ls <- readIORef $ fLayers f
	writeIORef (fLayers f) (ls ++ [[]])
	modifyIORef (fBuffed f) (++ [return ()])
	return Layer{layerField = f, layerId = length ls}

addCharacter :: Field -> IO Character
addCharacter f = do
	cs <- readIORef $ fCharacters f
	writeIORef (fCharacters f) (cs ++ [return ()])
	return Character{characterField = f, characterId = length cs}

drawLine :: Layer -> Double -> Double -> Double -> Double -> IO ()
drawLine l@Layer{layerField = f} x1 y1 x2 y2 = do
	drawLineBuf f fBG x1 y1 x2 y2 >> redrawCharacters f
	addLayerAction l $ whether (drawLineBuf f fUndoBuf x1 y1 x2 y2)
		(drawLineBuf f fBG x1 y1 x2 y2 >> redrawCharacters f)

undoN :: Int
undoN = 100

addLayerAction :: Layer -> (Bool -> IO ()) -> IO ()
addLayerAction Layer{layerField = f, layerId = lid} act = do
	ls <- readIORef $ fLayers f
	if length (ls !! lid) > undoN
		then do	head (ls !! lid) True
			buffed <- readIORef $ fBuffed f
			writeIORef (fBuffed f) $ 
				modifyAt buffed lid (>> head (ls !! lid) True)
			writeIORef (fLayers f) $
				modifyAt ls lid $ (++ [act]) . tail
		else writeIORef (fLayers f) $ modifyAt ls lid (++ [act])

drawCharacter :: Character -> [(Double, Double)] -> IO ()
drawCharacter c = setCharacter c . fillPolygonBuf (characterField c)

drawCharacterAndLine ::	Character -> [(Double, Double)] ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine c@Character{characterField = f} ps x1 y1 x2 y2 =
	setCharacter c $ fillPolygonBuf f ps >> drawLineBuf f fBuf x1 y1 x2 y2

setCharacter :: Character -> IO () -> IO ()
setCharacter Character{characterField = f, characterId = cid} act = do
	cs <- readIORef $ fCharacters f
	writeIORef (fCharacters f) $ setAt cs cid act
	redrawCharacters f
	flushWin f

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf f ps_ = do
	ps <- convertPos f ps_
	fillPolygon (fDisplay f) (fBuf f) (fGC f) (map (uncurry Point) ps)
		nonconvex coordModeOrigin

drawLineBuf :: Field -> (Field -> Pixmap) ->
	Double -> Double -> Double -> Double -> IO ()
drawLineBuf f@Field{fDisplay = dpy, fGC = gc} bf x1_ y1_ x2_ y2_ = do
	[(x1, y1), (x2, y2)] <- convertPos f [(x1_, y1_), (x2_, y2_)]
	X.drawLine dpy (bf f) gc x1 y1 x2 y2

convertPos :: Field -> [(Double, Double)] -> IO [(Position, Position)]
convertPos f ps = do
	(width, height) <- fieldSize f
	return $ (round . (+ width / 2) *** round . (+ height / 2) . negate)
		`map` ps

undoLayer :: Layer -> IO ()
undoLayer Layer{layerField = w, layerId = lid} = do
	ls <- readIORef $ fLayers w
	writeIORef (fLayers w) $ modifyAt ls lid init
	redraw w

clearLayer :: Layer -> IO ()
clearLayer Layer{layerField = f, layerId = lid} = do
	ls <- readIORef $ fLayers f
	writeIORef (fLayers f) $ setAt ls lid []
	buffed <- readIORef $ fBuffed f
	writeIORef (fBuffed f) $ setAt buffed lid $ return ()
	redrawAll f

redrawAll :: Field -> IO ()
redrawAll f = do
	redrawBuf f
	redraw f
	flushWin f

redrawBuf :: Field -> IO ()
redrawBuf f = do
	fieldSizeRaw f >>=
		uncurry (fillRectangle (fDisplay f) (fUndoBuf f) (fGCBG f) 0 0)
	readIORef (fBuffed f) >>= sequence_

redraw :: Field -> IO ()
redraw w = do
	(width, height) <- fieldSizeRaw w
	copyArea (fDisplay w) (fUndoBuf w) (fBG w) (fGC w) 0 0 width height 0 0
	readIORef (fLayers w) >>= mapM_ ($ False) . concat
	readIORef (fCharacters w) >>= sequence_

redrawCharacters :: Field -> IO ()
redrawCharacters f = do
	(width, height) <- fieldSizeRaw f
	copyArea (fDisplay f) (fBG f) (fBuf f) (fGC f) 0 0 width height 0 0
	readIORef (fCharacters f) >>= sequence_

flushWin :: Field -> IO ()
flushWin f = do
	(width, height) <- fieldSizeRaw f
	copyArea (fDisplay f) (fBuf f) (fWindow f) (fGC f) 0 0 width height 0 0
	flush $ fDisplay f
