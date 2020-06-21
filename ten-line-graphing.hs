import Graphics.UI.GLUT
main = do
  getArgsAndInitialize
  createWindow "OpenGL Graphing in Haskell"
  displayCallback $= display
  mainLoop
points = let {res=1000::GLfloat;pts=[(2*k/res-1,sin(2*pi*k/res+0.5),0::GLfloat)|k<-[1..res]]}
  in ((-1.1,0,0)::(GLfloat,GLfloat,GLfloat)):pts++[(1.1,0,0)::(GLfloat, GLfloat, GLfloat)]
display = let run = renderPrimitive LineLoop $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points
  in do clear [ ColorBuffer ]; run; flush
