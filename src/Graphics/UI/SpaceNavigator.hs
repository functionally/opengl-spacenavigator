{-|
Module      :  Graphics.UI.SpaceNavigator
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for managing input from a SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>, or a 3D mouse compatible with its protocols.  OpenGL callbacks are provided, along with utilities for quantizing the input from the mouse or tracking its six degrees of freedom.

Here is a simple example illustating the use of this module:

@
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
@

This code has been validated with the following configuration of hardware and software:

*   SpaceNavigator \<<http://www.3dconnexion.com/products/spacemouse/spacenavigator.html>\>

*   spacenavd \<<http://spacenav.sourceforge.net/>\>, 0.5

*   Ubuntu 15.04, 64-bit

*   GHC 7.6.3

*   OpenGL == 2.8.0.0

*   GLUT == 2.4.0.0
-}


{-# LANGUAGE RecordWildCards #-}


module Graphics.UI.SpaceNavigator (
-- * Input
  SpaceNavigatorInput(..)
, Button(..)
, ButtonAction(..)
, SpaceNavigatorCallback
, spaceNavigatorCallback
-- * Quantization
, quantize
, defaultQuantization
-- * Tracking
, Track(..)
, TrackMode(..)
, defaultTracking
, track
, doTracking
, doTracking'
) where


import Control.Applicative ((<*>), (<$>))
import Control.Monad (when)
import Data.Default (Default(..))
import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (GLfloat, SettableStateVar, Vector3(..), ($=!), ($~!), get, makeSettableStateVar, rotate, translate)
import Graphics.UI.GLUT (KeyState(..), SpaceballInput(..), spaceballCallback)


-- | Input received from a SpaceNavigator 3D mouse.
data SpaceNavigatorInput =
      -- | The mouse has been pushed.
      Push
      {
        pushRightward :: GLfloat       -- ^ The amount of rightward push, from -1 to +1.
      , pushUpward    :: GLfloat       -- ^ The amount of upward push, from -1 to +1.
      , pushBackward  :: GLfloat       -- ^ The amount of backward push, from -1 to +1.
      }
      -- | The mouse has been tilted.
    | Tilt
      {
        tiltForward   :: GLfloat       -- ^ The amount of forward tilt, from -1 to +1.
      , tiltClockwise :: GLfloat       -- ^ The amount of clockwise twist, from -1 to +1.
      , tiltRightward :: GLfloat       -- ^ The amount of rightward tilt, from -1 to +1.
      }
      -- | A mouse button has been pressed.
    | Button
      {
        buttonPress  :: Button         -- ^ Which button has been pressed.
      , buttonAction :: ButtonAction   -- ^ Whether the button has been pressed or released.
      }
      deriving (Eq, Read, Show)


-- | Buttons on a SpaceNavigator 3D mouse.
data Button =
    ButtonLeft       -- ^ The left button.
  | ButtonRight      -- ^ The right button.
  | ButtonOther Int  -- ^ Neither the left nor the right button.
    deriving (Eq, Read, Show)


-- | Pressing and releasing actions on a SpaceNavigator 3D mouse.
data ButtonAction =
    ButtonPress   -- ^ The button has been pressed.
  | ButtonRelease -- ^ The button has been released.
    deriving (Eq, Read, Show)


-- | Interpret SpaceBall input as SpaceNavigator input.
interpretSpaceball :: SpaceballInput      -- ^ The SpaceBall input.
                   -> SpaceNavigatorInput -- ^ The corresponding SpaceNavigator input.
interpretSpaceball (SpaceballMotion rightward upward forward) =
  Push
  {
    pushRightward =   fromIntegral rightward / 1000
  , pushUpward    =   fromIntegral upward    / 1000
  , pushBackward  = - fromIntegral forward   / 1000
  }
interpretSpaceball (SpaceballRotation backward counterClockwise rightward) =
  Tilt
  {
    tiltForward   = - fromIntegral backward         / 1800
  , tiltClockwise = - fromIntegral counterClockwise / 1800
  , tiltRightward =   fromIntegral rightward        / 1800
  }
interpretSpaceball (SpaceballButton button keyState) =
  Button
  {
    buttonPress  = case button of
                     0 -> ButtonLeft
                     1 -> ButtonRight
                     i -> ButtonOther i
  , buttonAction = case keyState of
                     Down -> ButtonPress
                     Up   -> ButtonRelease
  }


-- | A callback for input from the SpaceNavigator 3D mouse.
type SpaceNavigatorCallback = SpaceNavigatorInput -> IO ()


-- | Register the callback for input from the SpaceNavigator 3D mouse.
spaceNavigatorCallback :: SettableStateVar (Maybe SpaceNavigatorCallback)
spaceNavigatorCallback =
  makeSettableStateVar setSpaceNavigatorCallback
    where
      setSpaceNavigatorCallback :: Maybe SpaceNavigatorCallback -> IO ()
      setSpaceNavigatorCallback Nothing         = spaceballCallback $=! Nothing
      setSpaceNavigatorCallback (Just callback) = spaceballCallback $=! Just (callback . interpretSpaceball)


-- | Quantize the input from a SpaceNavigator 3D mouse according to whether the input exceeds a threshold.  The quantized input is -1, +1, or 0, depending on whether a threshold is exceeded.
quantize:: (GLfloat, GLfloat)     -- ^ The thresholds for pushing and titling, respectively, between 0 and +1.
        -> SpaceNavigatorCallback -- ^ The callback for the mouse.
        -> SpaceNavigatorCallback -- ^ A callback that receives quantized input {-1, 0, +1}.
quantize (pushThreshold, tiltThreshold) callback input =
  do
    let
      quantize' threshold v
        | v >   threshold' =  1
        | v < - threshold' = -1
        | otherwise        =  0
          where threshold' = abs threshold
      input' =
        case input of
          Push x y z -> Push (quantize' pushThreshold x) (quantize' pushThreshold y) (quantize' pushThreshold z)
          Tilt x y z -> Tilt (quantize' tiltThreshold x) (quantize' tiltThreshold y) (quantize' tiltThreshold z)
          b                        -> b
      report =
        case input' of
          Push x y z -> any (/= 0) [x, y, z]
          Tilt x y z -> any (/= 0) [x, y, z]
          _                        -> True
    when report
      $ callback input'


-- | A default quantization for the SpaceNavigator 3D mouse.
defaultQuantization :: (GLfloat, GLfloat)
defaultQuantization = (0.2, 0.1)


-- | Tracking information for a SpaceNavigator 3D mouse.
data Track =
  Track
  {
    trackMode        :: TrackMode       -- ^ The tracking mode.
  , trackPosition    :: Vector3 GLfloat -- ^ The coordinates for the position.
  , trackOrientation :: Vector3 GLfloat -- ^ The Euler angles for the orientation: yaw\/heading, pitch\/elevation, and roll\/bank, relative an initial orientation where the /-z/ axis is forward: see \<<https://en.wikipedia.org/wiki/Euler_angles#Alternative_names>\>.
  , trackLeftPress   :: Bool            -- ^ Whether the left button is pressed.
  , trackRightPress  :: Bool            -- ^ Whether the right button is pressed.
  }
    deriving (Eq, Read, Show)

instance Default Track where
  def = Track def (Vector3 0 0 0) (Vector3 0 0 0) False False


-- | The mode for tracking a SpaceNavigator 3D mouse.
--
-- /Currently only one mode is available, but other modes, such as flying and examining, will be implemented in the future./
data TrackMode =
  -- | Track the mouse as a \"platform\" in 3D space:
  --
  --   [push rightward] increment /x/ position
  --
  --   [push leftward] decrement /x/ position
  --
  --   [pull upward] increment /z/ position
  --
  --   [push downward] decrement /z/ position
  --
  --   [pull backward] increment /z/ position
  --
  --   [push forward] decrement /z/ position
  --
  --   [tilt leftward] increment first Euler angle, yaw\/heading
  --
  --   [tilt rightward] decrement first Euler angle, yaw\/heading
  --
  --   [twist counterclockwise] increment second Euler angle, pitch\/elevation
  --
  --   [twist clockwise] decrement second Euler angle, pitch\/elevation
  --
  --   [tilt backward] increment third Euler angle, roll\/bank
  --
  --   [tilt forward] decrement third Euler angle, roll\/bank
  TrackPlatform
    deriving (Eq, Read, Show)

instance Default TrackMode where
  def = TrackPlatform


-- | Track the movement of a SpaceNavigator 3D mouse.
track :: (Vector3 GLfloat, Vector3 GLfloat) -- ^ The rates at which to push or tilt, respectively, based on the mouse input.
      -> IORef Track                        -- ^ A reference to the tracking information.
      -> SpaceNavigatorCallback             -- ^ A callback for doing the tracking.
track (pushRates, _) tracking Push{..} =
  tracking $~!
    \t@Track{..} ->
      t {
          trackPosition = (pushRates `scale3` Vector3 pushRightward pushUpward pushBackward) `translate3` trackPosition
        }
track (_, tiltRates) tracking Tilt{..} =
  tracking $~!
    \t@Track{..} ->
      t {
          trackOrientation = (tiltRates `scale3` Vector3 (-tiltRightward) (-tiltClockwise) (-tiltForward)) `translate3` trackOrientation
        }
track _ tracking (Button ButtonLeft action) =
  tracking $~!
    \t ->
      t {
          trackLeftPress = action == ButtonPress
        }
track _ tracking (Button ButtonRight action) =
  tracking $~!
    \t ->
      t {
          trackRightPress = action == ButtonPress
        }
track _ _ (Button (ButtonOther _) _) = return ()


-- | Translate a 3-vector.
translate3 :: Num a
           => Vector3 a -- ^ The first vector.
           -> Vector3 a -- ^ The second vector.
           -> Vector3 a -- ^ The vector sum.
translate3 dv v = (+) <$> v <*> dv


-- | Scale a 3-vector.
scale3 :: Num a
       => Vector3 a -- ^ The first vector.
       -> Vector3 a -- ^ The second vector.
       -> Vector3 a -- ^ The vector with the production of the corresponding components.
scale3 s v = (*) <$> v <*> s


-- | Default tracking rates for the SpaceNavigator 3D mouse.
defaultTracking :: (Vector3 GLfloat, Vector3 GLfloat)
defaultTracking = (Vector3 0.01 0.01 0.01, Vector3 1 1 1)


-- | Return an action to track a SpaceNavigator 3D mouse via OpenGL matrices.
--
-- This simply calls @glTranslate@ on the position, followed by calls to @glRotate@ for the third Euler angle (roll\/bank) around the /x/-axis, the second (pitch\/elevation) around the /y/-axis, and then the first (yaw\/heading) around the /z/-axis, relative to an initial orientation where the /-z/ axis is forward.
doTracking :: Track -- ^ The tracking information.
           -> IO () -- ^ An action to track the mouse.
doTracking Track{..} =
  do
    let
      Vector3 alpha beta gamma = trackOrientation
    translate trackPosition
    rotate gamma $ Vector3 1 0 0
    rotate beta  $ Vector3 0 1 0
    rotate alpha $ Vector3 0 0 1


-- | Return an action to track a SpaceNavigator 3D mouse via OpenGL matrices.
--
-- This simply calls @glTranslate@ on the position, followed by calls to @glRotate@ for the third Euler angle (roll\/bank) around the /x/-axis, the second (pitch\/elevation) around the /y/-axis, and then the first (yaw\/heading) around the /z/-axis, relative to an initial orientation where the /-z/ axis is forward.
doTracking' :: IORef Track -- ^ A reference to the tracking information.
            -> IO ()       -- ^ An action to track the mouse.
doTracking' = (doTracking =<<) . get
