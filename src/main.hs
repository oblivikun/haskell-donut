module Main where

import Data.Vector
import System.Console.Terminal.Size
import Data.Bifunctor
import Data.Ix
import Data.List.Split

type NormalVector = Vector
type Brightness = Float
type PointOnShape = (Vector Float, NormalVector Float)
type PointOnSurface = (Vector Float, Brightness)
type PositionOnScreen = (Int, Int)
type RenderablePoint = (PositionOnScreen, Char)
type Grid = [[Char]]

constantK :: Float 
constantK = 18
constantK2 :: Float 
constantK2 = 6.5

constantR :: Float
constantR = 1.2
constantR2 :: Float
constantR2 = 2.5

createCirclePoints :: [PointOnShape]
createCirclePoints = map createPoint [0.0,0.1..2*pi]
 where
    createPoint :: Float -> PointOnShape
    createPoint angle = (Vector constantR2 0 0 + calculateRadius angle, calculateNormal angle)

    calculateRadius :: Float -> Vector Float
    calculateRadius angle = Vector (constantR * cos angle) (constantR * sin angle) 0

    calculateNormal :: Float -> NormalVector Float
    calculateNormal angle = Vector (cos angle) (sin angle) 0

createTorusPoints :: Float -> Float -> [PointOnSurface]
createTorusPoints a b = concatMap circle [0.0,0.1..2*pi]
 where
    sin_A = sin a
    sin_B = sin b
    cos_A = cos a
    cos_B = cos b

    circle :: Float -> [PointOnSurface]
    circle phi = map (calculateBrightness . bimap rotate rotate) createCirclePoints
      where
        sin_phi = sin phi
        cos_phi = cos phi

        rotate :: Vector Float -> Vector Float
        rotate (Vector x y z) = Vector
          ((x * (cos_B * cos_phi + sin_A * sin_B * sin_phi)) - y * cos_A * cos_B)
          ((x * (sin_B * cos_phi - cos_A * sin_B * sin_phi)) + y * cos_A * cos_B)
          (x * cos_A * sin_phi + y * sin_A)

calculateBrightness :: PointOnShape -> PointOnSurface
calculateBrightness (point, Vector nx ny nz) = (point, nx + ny + nz)

convertBrightnessToCharacter :: Brightness -> Char
convertBrightnessToCharacter brightness
 | brightness > 0     = chars !! min (floor (brightness*8)) 11
 | otherwise = ' '
 where 
    sqrt2 = 1.4142135624
    chars = ".,-~:;=!*#$@"
    maxIndex = fromIntegral (length chars - 1)
    index = floor $ brightness * maxIndex / sqrt2

sortByDepth :: [(Vector Float, RenderablePoint)] -> [RenderablePoint]
sortByDepth coordinates = map (resolveConflicts . findConflicts) $ [ (x, y) | y <- take 24 [0..], x <- take 40 [0..] ]
 where
    findConflicts :: PositionOnScreen -> [(Vector Float, RenderablePoint)]
    findConflicts position
      | null filtered = [(Vector 0 0 0, (position, ' '))]
      | otherwise     = filtered
      where filtered = filter ((== position) . fst . snd) coordinates
      
    resolveConflicts :: [(Vector Float, RenderablePoint)] -> RenderablePoint
    resolveConflicts = snd . foldl1 chooseHigher
      where 
        chooseHigher v@(Vector x y z,_) v'@(Vector _ _ maxZ,_)
          | z > maxZ = v
          | otherwise = v'

transform3Dto2D :: Vector Float -> PositionOnScreen
transform3Dto2D v@(Vector _ _ z) = pair . fmap (round . (* (constantK/(constantK2 + z)))) $ v
 where pair (Vector x y _) = (x+20, y+10)

arrangePointsInGrid :: [RenderablePoint] -> Grid
arrangePointsInGrid = chunksOf 80 . concatMap ((\c -> c:[c]) . snd)

displayTorus :: Float -> Float -> [[Char]]
displayTorus a b = do
 let torus = createTorusPoints a b
 let renderable = map (\(pos, brightness) -> (pos, (transform3Dto2D pos, convertBrightnessToCharacter brightness))) torus
 let sortedByDepth = sortByDepth renderable

 arrangePointsInGrid sortedByDepth

animateTorus :: Float -> Float -> IO ()
animateTorus a b = do
 putStr "\x1b[H"
 putStr . init . unlines $ displayTorus a b
 animateTorus (a+0.1) (b-0.1)

main :: IO ()
main = do
 window <- size
 window <- case window of
    Just a -> return a
    Nothing -> return $ Window 80 24

 animateTorus (2/pi) 0
