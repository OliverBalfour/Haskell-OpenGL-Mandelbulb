import Graphics.UI.GLUT

main :: IO ()
main = do
  (progName, args) <- getArgsAndInitialize
  initialWindowSize $= Size 640 480
  window <- createWindow "OpenGL Graphing in Haskell"
  displayCallback $= display
  mainLoop

no_pts :: GLfloat
no_pts = 1000

type Point = (GLfloat, GLfloat, GLfloat)
type Interval = (Float, Float)

-- gets a list of normalised points given a function and its domain and range
points :: (Float -> Float) -> Interval -> Interval -> [Point]
points func dom ran = let
  -- handle domain/range translation to screen [-1,1]^2 coords
  scale_to_screen intv x = 2 * (x - fst intv) / (snd intv - fst intv) - 1
  scale_to_graph intv x = (snd intv - fst intv) * x + fst intv
  -- get points
  point x = (scale_to_screen dom x, scale_to_screen ran $ func x, 0::GLfloat)
  pts = [point $ scale_to_graph dom $ t / no_pts | t <- [1..no_pts]]
  -- abuse the loop feature to draw axes as well
  axes = (scale_to_screen dom 0, scale_to_screen ran 0)
  axes_pts=let o=1.1 in [(o,o,0),(fst axes,o,0),(fst axes,-o,0),(o,-o,0),(o,snd axes,0)]::[Point]
  in ((-1.1,snd axes,0)::Point) : pts ++ axes_pts

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderPrimitive LineLoop $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z)
    $ points (\x -> x*sin(2*pi*x)) (-1,3) (-4,3)
  flush
