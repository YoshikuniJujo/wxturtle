module Graphics.X11.Turtle.FieldTools(
	openWindow,
	drawLine,
	drawCharacter,
	drawCharacterAndLine,
	fieldColor,
	writeString,

	module Graphics.X11.Turtle.FieldType
) where

import Graphics.X11 hiding (Color, drawLine)
import qualified Graphics.X11 as X
import Graphics.X11.Xim
import Graphics.X11.Xft
import Graphics.X11.Xrender

import Graphics.X11.Turtle.FieldType

import Text.XML.YJSVG(Color(..))
import Control.Monad
import Control.Monad.Tools
import Data.Bits
import Data.IORef
import System.Locale.SetLocale

openWindow :: IO (Display, Window, GC, GC, [Pixmap], XIC, Atom,
	IORef Dimension, IORef Dimension)
openWindow = do
	_ <- setLocale LC_CTYPE Nothing >>= maybe (error "Can't set locale.") return
	_ <- initThreads
	unlessM supportsLocale $ error "Current locale is notSupported."
	_ <- setLocaleModifiers ""
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
	im <- openIM dpy Nothing Nothing Nothing
	ic <- createIC im [XIMPreeditNothing, XIMStatusNothing] win
	fevent <- getICValue ic "filterEvents"
	[gc, gcBG] <- replicateM 2 $ createGC dpy win
	setForeground dpy gcBG 0xffffff
	forM_ bufs $ \bf -> fillRectangle dpy bf gcBG 0 0 rWidth rHeight
	setWMProtocols dpy win [del]
	selectInput dpy win $
		exposureMask .|. keyPressMask .|.
		buttonPressMask .|. buttonReleaseMask .|. button1MotionMask .|.
		fevent
	mapWindow dpy win
	[widthRef, heightRef] <- mapM newIORef [rWidth, rHeight]
	return (dpy, win, gc, gcBG, bufs, ic, del, widthRef, heightRef)

drawLine :: Field ->
	Layer -> Double -> Color -> Double -> Double -> Double -> Double -> IO ()
drawLine f l lw clr x1 y1 x2 y2 =
	addLayerAction l (drawLineBuf f (round lw) clr fUndoBuf x1 y1 x2 y2,
		drawLineBuf f (round lw) clr fBG x1 y1 x2 y2)

writeString :: Field -> Layer -> String -> Double -> Color ->
	Double -> Double -> String -> IO ()
writeString f l fname size clr x y str =
	addLayerAction l (writeStringBuf f fUndoBuf fname size clr x y str,
		writeStringBuf f fBG fname size clr x y str)

drawCharacter :: Field -> Character -> Color -> [(Double, Double)] -> IO ()
drawCharacter f c cl sh = setCharacter c $ do
	clr <- getColorPixel (fDisplay f) cl
	setForeground (fDisplay f) (fGC f) clr
	fillPolygonBuf f sh

drawCharacterAndLine ::	Field -> Character -> Color -> [(Double, Double)] -> Double ->
	Double -> Double -> Double -> Double -> IO ()
drawCharacterAndLine f c cl ps lw x1 y1 x2 y2 = setCharacter c $ do
	clr <- getColorPixel (fDisplay f) cl
	setForeground (fDisplay f) (fGC f) clr
	fillPolygonBuf f ps >> drawLineBuf f (round lw) cl fBuf x1 y1 x2 y2

drawLineBuf :: Field -> Int -> Color -> (Field -> Pixmap) ->
	Double -> Double -> Double -> Double -> IO ()
drawLineBuf f@Field{fDisplay = dpy, fGC = gc} lw c bf x1_ y1_ x2_ y2_ = do
	clr <- getColorPixel dpy c
	setForeground (fDisplay f) (fGC f) clr
	setLineAttributes (fDisplay f) (fGC f) (fromIntegral lw) lineSolid capRound joinRound
	[(x1, y1), (x2, y2)] <- convertPos f [(x1_, y1_), (x2_, y2_)]
	X.drawLine dpy (bf f) gc x1 y1 x2 y2

fillPolygonBuf :: Field -> [(Double, Double)] -> IO ()
fillPolygonBuf f ps_ = do
	ps <- convertPos f ps_
	fillPolygon (fDisplay f) (fBuf f) (fGC f) (map (uncurry Point) ps)
		nonconvex coordModeOrigin

fieldColor :: Field -> Color -> IO ()
fieldColor f@Field{fDisplay = dpy} c = do
	clr <- getColorPixel dpy c
	setForeground (fDisplay f) (fGCBG f) clr
	let bufs = [fUndoBuf f, fBG f, fBuf f]
	width <- readIORef $ fWidth f
	height <- readIORef $ fHeight f
	forM_ bufs $ \bf -> fillRectangle (fDisplay f) bf (fGCBG f) 0 0 width height

getColorPixel :: Display -> Color -> IO Pixel
getColorPixel _ (RGB r g b) = return $ shift (fromIntegral r) 16 .|.
	shift (fromIntegral g) 8 .|. fromIntegral b
getColorPixel dpy (ColorName cn) = do
	let	scr = defaultScreen dpy
		colormap = defaultColormap dpy scr
	fmap (X.color_pixel . fst) $ allocNamedColor dpy colormap cn

writeStringBuf :: Field -> (Field -> Pixmap) -> String -> Double ->
	Color -> Double -> Double -> String -> IO ()
writeStringBuf f buf fname size clr x_ y_ str = do
	let	dpy = fDisplay f
		scr = defaultScreen dpy
		scrN = defaultScreenOfDisplay dpy
		visual = defaultVisual dpy scr
		colormap = defaultColormap dpy scr
	xftDraw <- xftDrawCreate dpy (buf f) visual colormap
	xftFont <- xftFontOpen dpy scrN $ fname ++ "-" ++ show (round size :: Int)
	[(x, y)] <- convertPos f [(x_, y_)]
	withXftColor dpy visual colormap clr $ \c ->
		xftDrawString xftDraw c xftFont x y str

withXftColor ::
	Display -> Visual -> Colormap -> Color -> (XftColor -> IO a) -> IO a
withXftColor dpy visual colormap (RGB r g b) action =
	withXftColorValue dpy visual colormap color action
	where
	color = XRenderColor {
		xrendercolor_red = fromIntegral r * 0x100,
		xrendercolor_green = fromIntegral b * 0x100,
		xrendercolor_blue = fromIntegral g * 0x100,
		xrendercolor_alpha = 0xffff
	 }
withXftColor dpy visual colormap (ColorName cn) action =
	withXftColorName dpy visual colormap cn action