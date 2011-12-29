import Turtle
import Control.Monad

spiral :: Position -> Int -> IO ()
spiral size angle
	| size > 100	= return ()
	| otherwise	= do
		forward size
		right angle
		spiral (size + 2) angle

qcircle :: IO ()
qcircle = replicateM_ 9 $ forward 10 >> right 10

leaf :: IO ()
leaf = qcircle >> right 90 >> qcircle

flower :: IO ()
flower = do
	left 90
	forward 50
	clean
	replicateM_ 9 $ leaf >> right 10
	right 180
	forward 200
	right 180
	forward 30
	right 20
	leaf

triangles :: Position -> IO ()
triangles size
	| size < 10	= return ()
	| otherwise	= do
		right 60
		replicateM_ 3 $ forward size >> right 120
		forward $ size `div` 2
		triangles $ size `div` 2

polygon :: Position -> Int -> IO ()
polygon size repeats =
	replicateM_ repeats $ forward size >> right (360 `div` repeats)
