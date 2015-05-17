module Simulation (moveParticle, accelerate, advanceWorld) where

import World
import Physics

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle t (Particle m (p1,p2) (v1,v2)) = Particle m ((p1 + (v1 * t)),(p2 + (v2 * t))) (v1,v2)

-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--

accelerate :: Float -> [Particle] -> [Particle]
accelerate t ps = accelerate' t ps ps

accelerate' :: Float -> [Particle] -> [Particle] -> [Particle]
accelerate' t [] [] = []
accelerate' t [] qs = []
accelerate' t (p:ps) qs = (calcAndApplyForces t p qs):(accelerate' t ps qs)

calcAndApplyForces :: Float -> Particle -> [Particle] -> Particle
calcAndApplyForces t p ps = applyAccel p t (findAccel p ps)

findAccel :: Particle -> [Particle] -> [Accel]
findAccel p [] = []
findAccel p (q:qs) = (force p q):findAccel p qs

applyAccel :: Particle -> Float -> [Accel] -> Particle
applyAccel (Particle m (p1,p2) (v1,v2)) t as =
  let (a1,a2) = accelSum as
  in Particle m (p1,p2) ((v1 + (a1 * t)),(v2 + (a2 * t)))

accelSum :: [Accel] -> Accel
accelSum [] = (0.0,0.0)
accelSum [a] = a
accelSum (x:xs) = accelSum' x (accelSum xs)

accelSum' :: Accel -> Accel -> Accel
accelSum' (a, b) (c, d) = ((a+c), (b+d))

-- accelerate :: Float -> [Particle] -> [Particle]
-- accelerate t (p:ps) = accelerate' t (p:ps) (p:ps)
--
-- accelerate' :: Float -> [Particle] -> [Particle] -> [Particle]
-- accelerate' t (p:ps) [] = []
-- accelerate' t (p:ps) (q:qs) = (applyForce t (p:ps) q):(accelerate' t (p:ps) qs)
--
-- applyForce :: Float -> [Particle] -> Particle -> Particle
-- applyForce t (p:ps) (Particle m p' v) = Particle m p' (velSum v  (accelMul (sumAccel (map (force (Particle m p' v)) (p:ps))) (t,t)))
--
-- sumAccel :: [Accel] -> Accel
-- sumAccel [] = (0.0,0.0)
-- sumAccel [a] = a
-- sumAccel (x:xs) = accelSum x (sumAccel xs)
--
-- accelSum :: Accel -> Accel -> Accel
-- accelSum (a, b) (c, d) = ((a+c), (b+d))
--
-- accelMul :: Accel -> Accel -> Accel
-- accelMul (a, b) (c, d) = ((a*c), (b*d))
--
-- velSum :: Velocity -> Velocity -> Velocity
-- velSum (a, b) (c, d) = ((a+c), (b+d))
--
-- velMul :: Velocity -> Velocity -> Velocity
-- velMul (a, b) (c, d) = ((a*c), (b*d))

-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ t (World a b c (ps)) = World a b c (moveParticles (t*c) (accelerate (t*c) (ps)))

moveParticles :: Float -> [Particle] -> [Particle]
moveParticles t [] = []
moveParticles t (p:ps) = map (moveParticle t) (p:ps)
