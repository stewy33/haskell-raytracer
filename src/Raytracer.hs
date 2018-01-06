module Raytracer where

import Codec.Picture
import Data.List (foldl')
import Data.Word (Word16, Word8)
import Debug.Trace (traceShow, traceShowId)

inf = 1 / 0

cot :: Floating a => a -> a
cot x' = cos x / sin x
  where
    x = x' / 180 * pi

-- default round implementation of Word mods values but we don't want that
wordRound :: (RealFrac a, Ord a) => a -> Word16
wordRound a
  | a > fromIntegral m = m
  | otherwise = round a
  where
    m = maxBound :: Word16

nonNeg :: (Num a, Ord a) => a -> a
nonNeg a
  | a < 0 = 0
  | otherwise = a

-- Vector definition and utility functions
data Vec3 =
  Vec3 Double
       Double
       Double
  deriving (Eq, Show)

mult :: Double -> Vec3 -> Vec3
a `mult` (Vec3 x y z) = Vec3 (a * x) (a * y) (a * z)

dot :: Vec3 -> Vec3 -> Double
(Vec3 x1 y1 z1) `dot` (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

plus :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) `plus` (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

minus :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) `minus` (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

mag :: Vec3 -> Double
mag (Vec3 x y z) = sqrt $ x ^^ 2 + y ^^ 2 + z ^^ 2

invert :: Vec3 -> Vec3
invert = ((-1) `mult`)

-- normalizes vector to unit vector
normalize :: Vec3 -> Vec3
normalize v@(Vec3 x y z)
  | mag' == 0 = v -- to avoid divide by 0 error
  | otherwise = Vec3 (x / mag') (y / mag') (z / mag')
  where
    mag' = mag v

reflectionDir :: Vec3 -> Vec3 -> Vec3
reflectionDir dir norm = dir `minus` (2 `mult` dir `dot` norm `mult` norm)

data Ray = Ray
  { origin :: Vec3
  , direction :: Vec3
  } deriving (Eq, Show)

ray :: Vec3 -> Vec3 -> Ray
ray o dir = Ray o $ normalize dir

pointOnRay :: Double -> Ray -> Vec3
pointOnRay d (Ray o dir) = o `plus` (d `mult` dir)

data Camera = Camera
  { camPos :: Vec3
  , resolution :: (Int, Int)
  , fieldOfView :: Double
  }

type Color = PixelRGB16

color :: Word8 -> Word8 -> Word8 -> Color
color r g b = PixelRGB16 (f r) (f g) (f b)
  where
    f a = 257 * fromIntegral a

-- mix two colors together using the supplied function
-- uses Num and RealFrac to keep eliminate boilerplate
mix :: (Num a, Num b, RealFrac c) => (a -> b -> c) -> Color -> Color -> Color
mix f (PixelRGB16 r1 g1 b1) (PixelRGB16 r2 g2 b2) =
  PixelRGB16 (f' r1 r2) (f' g1 g2) (f' b1 b2)
  where
    f' a b = wordRound $ f (fromIntegral a) (fromIntegral b)

cmap :: (Num a, RealFrac b) => (a -> b) -> Color -> Color
cmap f (PixelRGB16 r g b) = PixelRGB16 (f' r) (f' g) (f' b)
  where
    f' = wordRound . f . fromIntegral

data Scene = Scene
  { background :: Color
  , camera :: Camera
  , lighting :: Lighting
  , objects :: [Object]
  }

data Lighting = Lighting
  { lightDirection :: Vec3
  , ambientLightFactor :: Double
  }

data Object = Object
  { material :: Mat
  , form :: Form
  }

data Form
  = Sphere { sphereCenter :: Vec3
           , radius :: Double }
  | Plane { planeCenter :: Vec3
          , planeNormal :: Vec3 }

data Mat = Mat
  { matColor :: Color
  , matType :: MatType
  }

data MatType
  = Diffuse { albedo :: Double }
  | Reflective { reflectivity :: Double }
  | Specular { shininess :: Int
             , specAlbedo :: Double
             , specularCoefficient :: Double }

getColor :: Maybe Object -> Ray -> Scene -> Int -> Color
-- if doesn't collide with any objects return background color
getColor Nothing _ scene _ = background scene
-- stop at maximum recursion depth
getColor _ _ scene 5 = PixelRGB16 0 0 0
-- for diffuse objects
getColor (Just (Object (Mat colo (Diffuse dc)) f)) (Ray pos _) scene n
  | isIlluminated pos lightDir (objects scene) = cmap darken colo
  | otherwise = cmap (ambient *) colo
  where
    Lighting lightDir ambient = lighting scene
    factor = nonNeg $ normal f pos `dot` lightDir
    darken a = ambient * a + dc * factor * a
-- for metallic objects
getColor (Just (Object (Mat colo (Reflective r)) f)) (Ray pos dir) scene n =
  mixedColor
  where
    mixedColor = mix (\refl c -> c * (1 - r) + r * refl) reflectColor colo
    reflectColor = getColor closestObj hitRay scene $ n + 1
    hitRay = Ray (pointOnRay closestHit reflRay) $ direction reflRay
    (closestHit, closestObj) = getClosestHit reflRay $ objects scene
    reflRay = Ray pos . reflectionDir dir $ normal f pos
-- for specular surfaces
getColor (Just (Object (Mat colo (Specular shin dc sc)) f)) r@(Ray pos dir) scene n =
  mixedColor
  where
    mixedColor =
      cmap
        (\c -> c + specFactor * fromIntegral (maxBound :: Word16))
        diffuseColor
    diffuseColor = getColor (Just (Object (Mat colo (Diffuse dc)) f)) r scene 0
    specFactor = sc * nonNeg (invert specDir `dot` dir) ^^ shin
    specDir = reflectionDir lightDir $ normal f pos
    lightDir = invert . lightDirection $ lighting scene

-- intersect takes Ray with normalized direction and Object
-- and returns distance on the ray where intersection occurs or a negative
-- number if no collision happens.
intersect :: Form -> Ray -> Double
intersect (Sphere c rad) (Ray orig dir)
  | otherLegMag < 0 || d > rad = -1
  | otherwise = otherLegMag - sqrt (rad ^^ 2 - d ^^ 2)
  where
    toSphere = c `minus` orig
    otherLegMag = toSphere `dot` dir
    d = sqrt $ mag toSphere ^^ 2 - otherLegMag ^^ 2
intersect (Plane c n) (Ray orig dir) = (c `minus` orig) `dot` n / dir `dot` n

normal :: Form -> Vec3 -> Vec3
normal (Sphere (Vec3 cx cy cz) r) (Vec3 x y z) =
  Vec3 ((x - cx) / r) ((y - cy) / r) ((z - cz) / r)
normal (Plane _ n) _ = n

-- returns true if point is illuminated
isIlluminated :: Vec3 -> Vec3 -> [Object] -> Bool
isIlluminated p lightDir os =
  not $ any (\o -> intersect (form o) shadowRay > 0.001) os
  where
    shadowRay = Ray p lightDir

getClosestHit :: Ray -> [Object] -> (Double, Maybe Object)
getClosestHit primRay = foldl' closerHit (inf, Nothing)
  where
    closerHit old@(oldClosestMag, oldObj) o =
      let d = intersect (form o) primRay
      in if d >= 0 && d < oldClosestMag
           then (d, Just o)
           else old

render :: Scene -> Image PixelRGB16
render scene@(Scene bg (Camera c (w, h) fov) _ os) =
  generateImage renderPixel w h
  where
    eyePos = c `minus` Vec3 0 0 1
    fovf = 2 * tan (fov / 2)
    renderPixel x' y' = getColor closestObj hitRay scene 0
      where
        x = (fromIntegral (x' - w `quot` 2) + 0.5) / fromIntegral w * fovf
        y =
          negate (fromIntegral (y' - h `quot` 2) + 0.5) / fromIntegral h * fovf
        pixelPos = c `plus` Vec3 x y 0
        primRay = ray pixelPos (pixelPos `minus` eyePos)
        -- get visible object's collision
        (closestHit, closestObj) = getClosestHit primRay os
        hitRay = Ray (pointOnRay closestHit primRay) $ direction primRay
