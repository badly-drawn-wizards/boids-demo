{-# LANGUAGE TupleSections, DataKinds, TypeApplications, DeriveGeneric #-}
module Main where

import Control.Lens
import Control.Applicative
import Control.Monad.Random.Class
import Data.Generics.Product
import Data.Monoid
import GHC.Generics hiding (to)
import Linear.V2
import Linear.Vector
import Linear.Metric
import Graphics.Gloss

data Boid = Boid
  { pos :: V2 Float
  , vel :: V2 Float
  , acc :: V2 Float

  , flockRadius :: Float
  , alignment :: Float -> Float
  , cohesion :: Float -> Float
  } deriving Generic

data Scene = Scene
  { boids :: [Boid]
  } deriving Generic

type TimeDelta = Float


_norm :: (Floating a, Eq a) => Traversal' (V2 a) a
_norm = filtered (/= zero) . lens norm (\ v d -> signorm v ^* d)

updateBoid :: (Foldable f) => TimeDelta -> f Boid -> (Boid -> Boid)
updateBoid dt boids boid = boid & updateFlocking & updatePhysics
  where

    isNeighbour :: Boid -> Boid -> Bool
    isNeighbour x y =  (pos x `distance` pos y) < flockRadius x

    neighbours :: Foldable f => Fold (f Boid) Boid
    neighbours = folded . filtered (isNeighbour boid)

    (center, heading) = boids
      & foldMapOf neighbours (\ b -> (Sum . pos $ b, Sum . vel $ b, Sum 1))
      & (\ (Sum totalPos, Sum totalHeading, Sum count) -> (totalPos ^/ fromIntegral count, totalHeading ^/ fromIntegral count))

    updateFlocking :: Boid -> Boid
    updateFlocking boid = boid
      & the @"acc" .~ sumV
        [ (center ^-^ pos boid) & _norm %~ (cohesion boid)
        , (heading ^-^ vel boid ) & _norm %~ (alignment boid)
        ]

    -- Euler integration. :-O
    updatePhysics :: Boid -> Boid
    updatePhysics boid = boid
      & the @"pos" +~ dt *^ (vel boid)
      & the @"vel" +~ dt *^ (acc boid)

updateBoids :: (Traversable t) => TimeDelta -> t Boid -> t Boid
updateBoids dt = selfIndex <. traversed %@~ updateBoid dt

updateScene :: TimeDelta -> Scene -> Scene
updateScene dt = the @"boids" %~ updateBoids dt


displayScene :: Scene -> Picture
displayScene = foldOf $ the @"boids"
  . folded
  . to displayBoid
  where
    displayBoid boid =
      let
        (V2 x y) = pos boid
      in
        translate x y $ Circle 20

generateScene :: MonadRandom m => m Scene
generateScene = [1..20] & traverseOf traversed (const generateBoid) & fmap Scene

generateBoid :: MonadRandom m => m Boid
generateBoid = do
    boidPos <- (V2 <$> getRandomR (0, 100) <*> getRandomR (0, 100))
    return $ Boid
      { pos = boidPos
      , vel = zero
      , acc = zero
      , flockRadius = 10
      , alignment = const 1
      , cohesion = const 1
      }


main :: IO ()
main = do
  init <- generateScene
  simulate FullScreen black 30 init displayScene (const updateScene)
