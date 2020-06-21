import Graphics.UI.GLUT
import Data.IORef

main :: IO ()
main = do
  (progName, args) <- getArgsAndInitialize
  initialWindowSize $= Size 640 640
  window <- createWindow "OpenGL Cube in Haskell"
  displayCallback $= display
  mainLoop

type Point = (GLfloat, GLfloat, GLfloat)

points :: GLfloat -> [Point]
points no_pts =
  let point x = (sin $ 2*pi*x, cos $ 2*pi*x, 0) :: Point
  in [point (t / no_pts) | t <- [1..no_pts]]

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex'
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

vertex' :: Point -> IO ()
vertex' (x, y, z) = vertex $ Vertex3 x y z

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  preservingMatrix $ do
    cube 0.1
  flush
