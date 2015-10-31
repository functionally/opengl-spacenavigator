Functions for using SpaceNavigator-compatible 3D Mice with OpenGL
=================================================================

This Haskell package contains functions for managing input from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>, or a 3D mouse compatible with its protocols.  OpenGL callbacks are provided, along with utilities for quantizing the input from the mouse or tracking its six degrees of freedom.


Skeletal example illustrating the use of a SpaceNavigator with OpenGL
---------------------------------------------------------------------

```haskell
main :: IO ()
main = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  _ <- createWindow \"SpaceNavigator OpenGL Example\"
  depthFunc $= Just Less 
  -- Create the tracker.
  tracking <- newIORef $ def {spaceNavigatorPosition = Vector3 0 0 0}
  -- Register a callback which quantizes and tracks the 3D mouse input.
  spaceNavigatorCallback $=! Just ( quantizeSpaceNavigator defaultQuantization $ trackSpaceNavigator defaultTracking tracking)
  -- The display callback needs the tracker.
  displayCallback $= display tracking
  idleCallback $= Just (postRedisplay Nothing)
  mainLoop

display :: IORef SpaceNavigatorTrack -> DisplayCallback
display tracking =
  do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    -- Get the tracking state.
    tracking' <- get tracking
    -- Update the matrix based on the tracking
    doTracking tracking'
    -- All of the rendering actions go here.
    renderPrimitive . . . 
    swapBuffers
```


Notes on hardware and software
------------------------------

This code has been validated with the following configuration of hardware and software:

*   SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>
*   spacenavd \<<http://spacenav.sourceforge.net/>\>, 0.5
*   Ubuntu 15.04, 64-bit
*   GHC 7.6.3
*   OpenGL == 2.8.0.0
*   GLUT == 2.4.0.0
