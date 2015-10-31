{-# LANGUAGE RecordWildCards #-}


module Graphics.UI.SpaceNavigator (
  SpaceNavigatorInput(..)
, SpaceNavigatorButton(..)
, SpaceNavigatorAction(..)
, SpaceNavigatorCallback
, spaceNavigatorCallback
, quantizeSpaceNavigator
, defaultQuantization
, SpaceNavigatorTrack(..)
, trackSpaceNavigator
, defaultTracking
, doTracking
) where


import Control.Applicative ((<*>), (<$>))
import Control.Monad (when)
import Data.Default (Default(..))
import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (GLfloat, Vector3(..), rotate, translate)
import Graphics.UI.GLUT (KeyState(..), SettableStateVar, SpaceballInput(..), ($=!), ($~!), makeSettableStateVar, spaceballCallback)


data SpaceNavigatorInput =
      SpaceNavigatorPush
      {
        pushRightward :: GLfloat
      , pushUpward    :: GLfloat
      , pushBackward  :: GLfloat
      }
    | SpaceNavigatorTilt
      {
        tiltForward   :: GLfloat
      , tiltClockwise :: GLfloat
      , tiltRightward :: GLfloat
      }
    | SpaceNavigatorButton
      {
        buttonPress  :: SpaceNavigatorButton
      , buttonAction :: SpaceNavigatorAction
      }
      deriving (Eq, Read, Show)


data SpaceNavigatorButton = SpaceNavigatorLeft | SpaceNavigatorRight | SpaceNavigatorOther Int
  deriving (Eq, Read, Show)


data SpaceNavigatorAction = SpaceNavigatorPress | SpaceNavigatorRelease
  deriving (Eq, Read, Show)


interpretSpaceball :: SpaceballInput -> SpaceNavigatorInput
interpretSpaceball (SpaceballMotion rightward upward forward) =
  SpaceNavigatorPush
  {
    pushRightward =   fromIntegral rightward / 1000
  , pushUpward    =   fromIntegral upward    / 1000
  , pushBackward  = -  fromIntegral forward   / 1000
  }
interpretSpaceball (SpaceballRotation backward counterClockwise rightward) =
  SpaceNavigatorTilt
  {
    tiltForward   = - fromIntegral backward         / 1800
  , tiltClockwise = - fromIntegral counterClockwise / 1800
  , tiltRightward =   fromIntegral rightward        / 1800
  }
interpretSpaceball (SpaceballButton button keyState) =
  SpaceNavigatorButton
  {
    buttonPress  = case button of
                     0 -> SpaceNavigatorLeft
                     1 -> SpaceNavigatorRight
                     i -> SpaceNavigatorOther i
  , buttonAction = case keyState of
                     Down -> SpaceNavigatorPress
                     Up   -> SpaceNavigatorRelease
  }


type SpaceNavigatorCallback = SpaceNavigatorInput -> IO ()


spaceNavigatorCallback :: SettableStateVar (Maybe SpaceNavigatorCallback)
spaceNavigatorCallback =
  makeSettableStateVar setSpaceNavigatorCallback
    where
      setSpaceNavigatorCallback :: Maybe SpaceNavigatorCallback -> IO ()
      setSpaceNavigatorCallback Nothing         = spaceballCallback $=! Nothing
      setSpaceNavigatorCallback (Just callback) = spaceballCallback $=! Just (callback . interpretSpaceball)


quantizeSpaceNavigator :: (GLfloat, GLfloat) -> SpaceNavigatorCallback -> SpaceNavigatorCallback
quantizeSpaceNavigator (pushThreshold, tiltThreshold) callback input =
  do
    let
      quantize threshold v
        | v >   threshold =  1
        | v < - threshold = -1
        | otherwise       =  0
      input' = 
        case input of
          SpaceNavigatorPush x y z -> SpaceNavigatorPush (quantize pushThreshold x) (quantize pushThreshold y) (quantize pushThreshold z)
          SpaceNavigatorTilt x y z -> SpaceNavigatorTilt (quantize tiltThreshold x) (quantize tiltThreshold y) (quantize tiltThreshold z)
          b                        -> b
      report =
        case input' of
          SpaceNavigatorPush x y z -> any (/= 0) [x, y, z]
          SpaceNavigatorTilt x y z -> any (/= 0) [x, y, z]
          _                        -> True
    when report
      $ callback input'


defaultQuantization :: (GLfloat, GLfloat)
defaultQuantization = (0.2, 0.1)


data SpaceNavigatorTrack =
  SpaceNavigatorTrack
  {
    spaceNavigatorPosition   :: Vector3 GLfloat
  , spaceNavigatorAngles     :: Vector3 GLfloat
  , spaceNavigatorLeftPress  :: Bool
  , spaceNavigatorRightPress :: Bool
  }
    deriving (Eq, Read, Show)


instance Default SpaceNavigatorTrack where
  def = SpaceNavigatorTrack (Vector3 0 0 0) (Vector3 0 0 0) False False


-- TODO: Add different tracking modes.
trackSpaceNavigator :: (Vector3 GLfloat, Vector3 GLfloat) -> IORef SpaceNavigatorTrack -> SpaceNavigatorCallback
trackSpaceNavigator (pushRates, _) tracking SpaceNavigatorPush{..} =
  tracking $~!
    \t@SpaceNavigatorTrack{..} ->
      t {
          spaceNavigatorPosition = (pushRates `scale3` Vector3 pushRightward pushUpward pushBackward) `translate3` spaceNavigatorPosition
        }
trackSpaceNavigator (_, tiltRates) tracking SpaceNavigatorTilt{..} =
  tracking $~!
    \t@SpaceNavigatorTrack{..} ->
      t {
          spaceNavigatorAngles = (tiltRates `scale3` Vector3 (-tiltRightward) (-tiltClockwise) (-tiltForward)) `translate3` spaceNavigatorAngles
        }
trackSpaceNavigator _ tracking (SpaceNavigatorButton SpaceNavigatorLeft action) =
  tracking $~!
    \t ->
      t {
          spaceNavigatorLeftPress = action == SpaceNavigatorPress
        }
trackSpaceNavigator _ tracking (SpaceNavigatorButton SpaceNavigatorRight action) =
  tracking $~!
    \t ->
      t {
          spaceNavigatorRightPress = action == SpaceNavigatorPress
        }
trackSpaceNavigator _ _ (SpaceNavigatorButton (SpaceNavigatorOther _) _) = return ()


translate3 :: Num a => Vector3 a -> Vector3 a -> Vector3 a
translate3 dv v = (+) <$> v <*> dv


scale3 :: Num a => Vector3 a -> Vector3 a -> Vector3 a
scale3 s v = (*) <$> v <*> s


defaultTracking :: (Vector3 GLfloat, Vector3 GLfloat)
defaultTracking = (Vector3 0.01 0.01 0.01, Vector3 1 1 1)


doTracking :: SpaceNavigatorTrack -> IO ()
doTracking SpaceNavigatorTrack{..} =
  do
    let
      Vector3 alpha beta gamma = spaceNavigatorAngles
    translate spaceNavigatorPosition
    rotate gamma $ Vector3 1 0 0
    rotate beta  $ Vector3 0 1 0
    rotate alpha $ Vector3 0 0 1
