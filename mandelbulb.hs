module Main where

import System.Exit
import System.IO
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core31 (glUniform1f, glUniform2f)

import Unsafe.Coerce(unsafeCoerce)
import GHC.Float (double2Float, int2Float)
maybeDoubleToGF :: Maybe Double -> GL.GLfloat
maybeDoubleToGF (Just d) = unsafeCoerce $ double2Float d
maybeDoubleToGF _ = 0.0
intToGF = unsafeCoerce . int2Float :: Int -> GL.GLfloat

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ =
  ($) when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed)
      (GLFW.setWindowShouldClose window True)

errorCallback :: GLFW.ErrorCallback
errorCallback _ = System.IO.hPutStrLn stderr

createWindow :: String -> Int -> Int -> IO GLFW.Window
createWindow title width height = do
  GLFW.setErrorCallback (Just errorCallback)
  init <- GLFW.init
  if not init then exitFailure else do
    GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
    win <- GLFW.createWindow width height title Nothing Nothing
    case win of
      Nothing -> GLFW.terminate >> exitFailure
      Just _win -> do
        GLFW.makeContextCurrent win
        GLFW.setKeyCallback _win (Just keyCallback)
        return _win

readFileBS :: String -> IO BS.ByteString
readFileBS name = do
  handle <- openFile name ReadMode
  contents <- BS.hGetContents handle
  hClose handle
  return contents

shaderModule :: String -> String -> IO GL.Program
shaderModule vshFilename fshFilename = do
  vsh <- compile vshFilename GL.VertexShader
  fsh <- compile fshFilename GL.FragmentShader
  program <- GL.createProgram
  GL.attachShader program vsh
  GL.attachShader program fsh
  -- set attribute locations
  GL.attribLocation program "coord" $= GL.AttribLocation 0
  GL.linkProgram program
  GL.validateProgram program
  validateLinkedCorrectly program
  return program
  where
    compile filename shaderType = do
      source <- readFileBS filename
      sh <- GL.createShader shaderType
      GL.shaderSourceBS sh GL.$= source
      GL.compileShader sh
      validateCompiledCorrectly sh
      return sh
    validateCompiledCorrectly sh = do
      result <- GL.get $ GL.compileStatus sh
      unless result $ do
        info <- GL.get $ GL.shaderInfoLog sh
        putStrLn $ "Error:" ++ info
        exitFailure
    validateLinkedCorrectly program = do
      a <- GL.get $ GL.linkStatus program
      b <- GL.get $ GL.validateStatus program
      unless (a && b) $ do
        info <- GL.get $ GL.programInfoLog program
        putStrLn $ "Error:" ++ info
        exitFailure

mainLoop :: IO () -> GLFW.Window -> IO ()
mainLoop draw window = do
  close <- GLFW.windowShouldClose window
  unless close $ do
    draw
    GLFW.swapBuffers window
    GLFW.pollEvents
    mainLoop draw window

draw :: GL.Program -> GLFW.Window -> IO ()
draw program window = do
  (width, height) <- GLFW.getFramebufferSize window
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  GL.clear [GL.ColorBuffer]
  GL.currentProgram $= Just program
  -- add vertices of a fullscreen covering triangle to render
  let vertices = (V.fromList [-1.0,-1.0,0, 3.0,-1.0,0, -1.0,3.0,0]) :: V.Vector Float
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  V.unsafeWith vertices $ \ptr -> do
      GL.vertexAttribPointer (GL.AttribLocation 0) $=
          (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
  -- add time uniform
  t <- GLFW.getTime
  GL.UniformLocation tLoc <- GL.get $ GL.uniformLocation program "time"
  glUniform1f tLoc (maybeDoubleToGF t)
  -- add resolution uniform
  GL.UniformLocation bLoc <- GL.get $ GL.uniformLocation program "size"
  glUniform2f bLoc (intToGF width) (intToGF height)
  -- draw
  GL.drawArrays GL.Triangles 0 3
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled

cleanup :: GLFW.Window -> IO ()
cleanup window = do
  GLFW.destroyWindow window
  GLFW.terminate
  exitSuccess

main :: IO ()
main = do
  window <- createWindow "Haskell OpenGL Mandelbulb" 640 480
  program <- shaderModule "mandelbulb.vsh" "mandelbulb.fsh"
  mainLoop (draw program window) window
  cleanup window
