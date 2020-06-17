import Graphics.UI.GLUT
main = do
  getArgsAndInitialize; createWindow "OpenGL Graphing in Haskell"; displayCallback $= display; mainLoop
points=((-1.1,0,0):[(k/50-1,sin(pi*k/50+0.5),0::GLfloat)|k<-[1..100]]++[(1.1,0,0)])::[(GLfloat,GLfloat,GLfloat)]
display = let run = renderPrimitive LineLoop $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points
  in do clear [ ColorBuffer ]; run; flush
