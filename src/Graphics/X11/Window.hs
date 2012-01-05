module Graphics.X11.Window (
	Win,
	openWindow,
	closeWindow,
	flushWindow,
	getWindowSize,
	cleanBG,
	cleanUndoBuf,
	lineToBG,
	lineToUndoBuf,
	makeFilledPolygonCursor,
	undoBufToBG,
	bgToBuf,
	bufToWin,

	testModuleWindow,
) where

import Graphics.X11
import Graphics.X11.Xlib.Extras
import Data.Bits
import Data.Char
import Data.Convertible
import Control.Monad.Tools
import Data.IORef
import Control.Concurrent
import System.IO.Unsafe

data Win = Win{
	wDisplay :: Display,
	wWindow :: Window,
	wGC :: GC,
	wDel :: Atom,
	wBG :: Pixmap,
	wBuf :: Pixmap,
	wUndoBuf :: Pixmap,
	wWidth :: Dimension,
	wHeight :: Dimension
 } deriving Show

winWidth, winHeight :: IORef Dimension
winWidth = unsafePerformIO $ newIORef $ error "winWidth undefined"
winHeight = unsafePerformIO $ newIORef $ error "winHeight undefined"

getWindowSize :: Win -> IO (Double, Double)
getWindowSize w = do
	width <- readIORef winWidth
	height <- readIORef winHeight
	return (fromIntegral width, fromIntegral height)

openWindow :: IO (Win, IORef (IO ()))
openWindow = do
	dpy <- openDisplay ""
	del <- internAtom dpy "WM_DELETE_WINDOW" True
	let	scr = defaultScreen dpy
	root <- rootWindow dpy scr
	(_, _, _, width, height, _, _) <- getGeometry dpy root
	writeIORef winWidth width
	writeIORef winHeight height
	let	black = blackPixel dpy scr
		white = whitePixel dpy scr
	win <- createSimpleWindow dpy root 0 0 width height 1 black white
	bg <- createPixmap dpy root width height $ defaultDepth dpy scr
	buf <- createPixmap dpy root width height $ defaultDepth dpy scr
	undoBuf <- createPixmap dpy root width height $ defaultDepth dpy scr
	gc <- createGC dpy win
	gc' <- createGC dpy win
	setForeground dpy gc' 0xffffff
	fillRectangle dpy bg gc' 0 0 width height
	fillRectangle dpy buf gc' 0 0 width height
	fillRectangle dpy undoBuf gc' 0 0 width height
	setWMProtocols dpy win [del]
	selectInput dpy win $ exposureMask .|. keyPressMask
	mapWindow dpy win
	flush dpy
	exposeAction <- newIORef (return ())
	let w = Win dpy win gc del bg buf undoBuf width height
	forkIO $ withEvent w () $ \() ev ->
		case ev of
			ExposeEvent{} -> do
				(_, _, _, width, height, _, _) <- getGeometry (wDisplay w) (wWindow w)
				writeIORef winWidth width
				writeIORef winHeight height
				putStrLn "expose event occur"
				readIORef exposeAction >>= id
				return ((), True)
			KeyEvent{} -> do
--				ch <- eventToChar w ev
				return ((), True)
--				return ((), ch /= 'q')
			ClientMessageEvent{} ->
				return ((), not $ isDeleteEvent w ev)
			_ -> -- error $ "not implemented for event " ++ show ev
				return ((), True)
	return (w, exposeAction)

closeWindow :: Win -> IO ()
closeWindow = closeDisplay . wDisplay

bgToBuf :: Win -> IO ()
bgToBuf w = do
	copyArea (wDisplay w) (wBG w) (wBuf w) (wGC w)
		0 0 (wWidth w) (wHeight w) 0 0

bufToWin :: Win -> IO ()
bufToWin w = do
	copyArea (wDisplay w) (wBuf w) (wWindow w) (wGC w)
		0 0 (wWidth w) (wHeight w) 0 0

undoBufToBG :: Win -> IO ()
undoBufToBG w = do
	copyArea (wDisplay w) (wUndoBuf w) (wBG w) (wGC w) 0 0 (wWidth w) (wHeight w) 0 0


withEvent :: Win -> s -> (s -> Event -> IO (s, Bool)) -> IO s
withEvent w stat0 act = doWhile stat0 $ \stat -> allocaXEvent $ \e -> do
	nextEvent (wDisplay w) e
	getEvent e >>= act stat

eventToChar :: Win -> Event -> IO Char
eventToChar w ev =
	fmap (chr . fromEnum) $ keycodeToKeysym (wDisplay w) (ev_keycode ev) 0

isDeleteEvent :: Win -> Event -> Bool
isDeleteEvent w ev@ClientMessageEvent{} = convert (head $ ev_data ev) == wDel w
isDeleteEvent _ _ = False

makeFilledPolygonCursor :: Win -> [(Double, Double)] -> IO ()
makeFilledPolygonCursor w ps =
	fillPolygon (wDisplay w) (wBuf w) (wGC w) (mkPs ps) nonconvex coordModeOrigin
	where
	doublesToPoint (x, y) = Point (round x) (round y)
	mkPs = map doublesToPoint

data Buf = BG | UndoBuf

lineToBG :: Win -> Double -> Double -> Double -> Double -> IO ()
lineToBG w = lineToGen w BG

lineToUndoBuf :: Win -> Double -> Double -> Double -> Double -> IO ()
lineToUndoBuf w = lineToGen w UndoBuf

lineToGen :: Win -> Buf -> Double -> Double -> Double -> Double -> IO ()
lineToGen w BG x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wBG w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]
lineToGen w UndoBuf x1_ y1_ x2_ y2_ = drawLine (wDisplay w) (wUndoBuf w) (wGC w) x1 y1 x2 y2
	where	[x1, y1, x2, y2] = map round [x1_, y1_, x2_, y2_]

cleanBG :: Win -> IO ()
cleanBG w = cleanGen w BG

cleanUndoBuf :: Win -> IO ()
cleanUndoBuf w = cleanGen w UndoBuf

cleanGen :: Win -> Buf -> IO ()
cleanGen w b = do
	gc <- createGC (wDisplay w) (wWindow w)
	setForeground (wDisplay w) gc 0xffffff
	let buf = case b of
		BG -> wBG w
		UndoBuf -> wUndoBuf w
	fillRectangle (wDisplay w) buf gc 0 0 (wWidth w) (wHeight w)

flushWindow :: Win -> IO ()
flushWindow = flush . wDisplay

testModuleWindow :: IO ()
testModuleWindow = main

main :: IO ()
main = do
	putStrLn "module Window"
	(w, _) <- openWindow
	withEvent w () $ \() ev ->
		case ev of
			ExposeEvent{} -> return ((), True)
			KeyEvent{} -> do
				ch <- eventToChar w ev
				return ((), ch /= 'q')
			ClientMessageEvent{} ->
				return ((), not $ isDeleteEvent w ev)
			_ -> error $ "not implemented for event " ++ show ev
	closeWindow w