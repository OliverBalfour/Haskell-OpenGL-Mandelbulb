module Main where

import System.Exit
import System.IO
import Control.Monad
import qualified Data.ByteString as BS

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

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

main :: IO ()
main = do
  window <- createWindow "Haskell OpenGL Mandelbulb" 640 480
  exitSuccess
