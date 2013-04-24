module Graphics.Rendering.Haray.HMDInfo where

import Numeric.LinearAlgebra.Vector ( Vec4(..) )

-- | This corresponds to the HMD info of the oculus rift see the developer
-- documentation at http://oculusvr.com

-- | HMDInfo is Head Mounted Display info.
data HMDInfo a = HMDInfo
  { hmdHScreenSize            :: !a   -- ^ Stored in meters
  , hmdVScreenSize            :: !a   -- ^ Stored in meters
  , hmdVScreenCenter          :: !a   -- ^ Currently VScreenSize/2
  , hmdEyeToScreenDistance    :: !a   -- ^ Stored in meters
  , hmdLensSeparationDistance :: !a   -- ^ Stored in meters
  , hmdInterpupillaryDistance :: !a   -- ^ Distance between centers of eyes
  , hmdHResolution            :: !Int -- ^ Horizontal resolution in pixels
  , hmdVResolution            :: !Int -- ^ Vertical resolution in pixels
  , hmdDistortionK            :: !(Vec4 a) -- ^ Radial distortion correction coefficients
  } deriving (Read, Show, Eq, Ord)

aspectRatio :: Fractional a => HMDInfo a -> a
aspectRatio hmdi = fromIntegral (hmdHResolution hmdi) / (2 * (fromIntegral (hmdVResolution hmdi)))

phiFov :: Floating a => HMDInfo a -> a
phiFov hmdi = 2 * atan (hmdVScreenSize hmdi / (2 * hmdEyeToScreenDistance hmdi))

