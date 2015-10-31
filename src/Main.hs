{-|
Module      :  Main
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Example application illustrating obtaining input from and tracking a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>, or a 3D mouse compatible with its protocols.
-}


module Main (
  -- * Entry Point
  main
) where


import Data.Default (def)
import Data.IORef (IORef, newIORef)
import Graphics.Rendering.OpenGL (ClearBuffer(..), Color3(..), ComparisonFunction(Less), GLfloat, MatrixMode(Modelview, Projection), Position(..), PrimitiveMode(..), Size(..), Vector3(..), Vertex3(..), ($=), ($=!), clear, color, depthFunc, flush, frustum, get, loadIdentity, matrixMode, preservingMatrix, renderPrimitive, rotate, translate, vertex, viewport)
import Graphics.UI.GLUT (DisplayCallback, DisplayMode(..), ReshapeCallback, createWindow, displayCallback, getArgsAndInitialize, idleCallback, initialDisplayMode, mainLoop, postRedisplay, reshapeCallback, swapBuffers)
import Graphics.UI.SpaceNavigator (Track(..), defaultQuantization, defaultTracking, doTracking', quantize, spaceNavigatorCallback, track)


-- | The main action.
main :: IO ()
main =
  do
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _ <- createWindow "SpaceNavigator "
    depthFunc $= Just Less 
    reshapeCallback $= Just reshape
    dispatch arguments


-- | Respond to the window being created or reshaped.
reshape :: ReshapeCallback
reshape (Size w h) = 
  do
    viewport $= (Position 0 0, Size (minimum [w, h]) (minimum[w, h]))
    matrixMode $=! Projection
    loadIdentity
    frustum (-0.4) 0.4 (-0.4) 0.4 1 6
    matrixMode $=! Modelview 0


-- | Respond to command-line arguments.
dispatch :: [String] -> IO ()

dispatch ["raw"] =
  do 
    spaceNavigatorCallback $=! Just print
    displayCallback $=! display Nothing
    mainLoop

dispatch ["quantized"] =
  let
     (pushThreshold, tiltThreshold) = defaultQuantization
  in
    dispatch ["quantized", show pushThreshold, show tiltThreshold]

dispatch ["quantized", pushThreshold, tiltThreshold] =
  do
    spaceNavigatorCallback $=! Just (quantize (read pushThreshold, read tiltThreshold) print)
    displayCallback $=! display Nothing
    mainLoop

dispatch ["track"] =
  let
     (pushThreshold, tiltThreshold) = defaultQuantization
  in
    dispatch ["track", show pushThreshold, show tiltThreshold]

dispatch ["track", pushThreshold, tiltThreshold] =
  do
    tracking <- newIORef $ def {trackPosition = Vector3 0 0 0}
    spaceNavigatorCallback $=! Just
      (
        quantize (read pushThreshold, read tiltThreshold)
          $ \input ->
            do
              track defaultTracking tracking input
              tracking' <- get tracking
              print tracking'
      )
    displayCallback $=! display (Just tracking)
    idleCallback $=! Just (postRedisplay Nothing)
    mainLoop

dispatch _ =
  do
    putStrLn "Usage:"
    putStrLn "  opengl-spacenavigator raw                                     : prints raw data from SpaceNavigator"
    putStrLn "  opengl-spacenavigator quantized [pushThreshold tiltThreshold] : prints quantized data from SpaceNavigator"
    putStrLn "  opengl-spacenavigator track [pushThreshold tiltThreshold]     : prints 6D tracking based on data from SpaceNavigator"


-- | The display callback.
display :: Maybe (IORef Track) -> DisplayCallback
display (Just tracking) =
  do
    let
      (u, v, w) = (0.03, 0.06, -0.18) :: (GLfloat, GLfloat, GLfloat)
      p0 = ( 0,  0, w)
      p1 = (-u, -u, 0)
      p2 = ( u, -u, 0)
      p3 = ( 0,  v, 0)
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    rotate 10 $ Vector3 1 2 (0 :: GLfloat)
    translate $ Vector3 0.5 (-0.2) (-3.5 :: GLfloat)
    preservingMatrix $ do
      doTracking' tracking
      color $ Color3 1.0 0.4 (0.5 :: GLfloat)
      renderPrimitive Triangles
        $ mapM_ vertex3f
        [
          p1, p2, p3
        , p1, p2, p0
        , p1, p0, p3
        , p0, p2, p3
        ]
      color $ Color3 1 1 (1 :: GLfloat)
      renderPrimitive Lines
        $ mapM_ vertex3f
        [
          p1, p2
        , p2, p3
        , p3, p1
        , p1, p0
        , p2, p0
        , p3, p0
        ]
    color $ Color3 0.5 1.0 (0.4 :: GLfloat)
    preservingMatrix $
      renderPrimitive Lines
        $ mapM_ vertex3f
        $ concat
        [
          [
            (2 * u' - 1, 2 * v' - 1,       -0.8), (2 * u' - 1, 2 * v' - 1,        0.8)
          , (2 * u' - 1,       -0.8, 2 * v' - 1), (2 * u' - 1,        0.8, 2 * v' - 1)
          , (      -0.8, 2 * u' - 1, 2 * v' - 1), (       0.8, 2 * u' - 1, 2 * v' - 1)
          ]
        |
          u' <- [0.1,0.2..0.9]
        , v' <- [0.1,0.2..0.9]
        ]
    flush
    swapBuffers

display Nothing =
  do
    clear [ColorBuffer, DepthBuffer]
    swapBuffers


-- | Make coordinates into a vertex.
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
