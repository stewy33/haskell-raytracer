module Main where

import Codec.Picture
import Raytracer

sceneObjects :: [Object]
sceneObjects = [aquaSphere, redSphere, blueSphere, greenSphere, ground]
  where
    aquaSphere =
      Object
      { material = Mat {matColor = color 0 190 190, matType = Diffuse 0.9}
      , form = Sphere {sphereCenter = Vec3 0 2 5, radius = 2}
      }
    redSphere =
      Object
      { material = Mat {matColor = color 250 0 0, matType = Diffuse 0.8}
      , form = Sphere {sphereCenter = Vec3 7.5 5 10, radius = 5}
      }
    blueSphere =
      Object
      { material = Mat {matColor = color 0 0 250, matType = Reflective 0.4}
      , form = Sphere {sphereCenter = Vec3 (-5) 4 8, radius = 4}
      }
    greenSphere =
      Object
      { material =
          Mat {matColor = color 255 200 177, matType = Specular 80 0.9 0.4}
      , form = Sphere {sphereCenter = Vec3 3 1.5 5, radius = 1.5}
      }
    ground =
      Object
      { material = Mat {matColor = color 250 250 250, matType = Diffuse 1}
      , form = Plane {planeCenter = Vec3 0 0 0, planeNormal = Vec3 0 1 0}
      }

main :: IO ()
main =
  writePng "example.png" $
  render
    Scene
    {background = bg, camera = cam, lighting = lights, objects = sceneObjects}
  where
    bg = color 30 100 250
    cam =
      Camera {camPos = Vec3 0 2 0, resolution = (2048, 2048), fieldOfView = 90}
    -- supply inverted direction of light
    lights =
      Lighting
      { lightDirection = invert . normalize $ Vec3 (-0.5) (-1) 0.5
      , ambientLightFactor = 0.2
      }
