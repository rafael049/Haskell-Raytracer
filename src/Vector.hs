module Vector where

data Vector3 a = Vector3
  { x :: a
  , y :: a
  , z :: a
  }
  deriving (Show)

type Vec3 = Vector3 Float


vec3 :: Float -> Float -> Float -> Vec3
vec3 x y z = Vector3 x y z

instance Functor Vector3 where
  fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Num a => Num (Vector3 a) where
  fromInteger x = Vector3 (fromInteger x) (fromInteger x) (fromInteger x)
  abs = fmap abs
  signum = fmap signum
  
  (Vector3 x1 y1 z1) + (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

  (Vector3 x1 y1 z1) - (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

  (Vector3 x1 y1 z1) * (Vector3 x2 y2 z2) = Vector3 (x1 * x2) (y1 * y2) (z1 * z2)

-- Right scalar multiplication
(*^) :: Float -> Vector3 Float -> Vector3 Float
t *^ v3 = fmap (*t) v3
-- Left scalar multiplication
(^*) :: Vector3 Float -> Float -> Vector3 Float
v3 ^* t = fmap (*t) v3

-- Rigth scalar division
(/^) :: Float -> Vector3 Float -> Vector3 Float
t /^ (Vector3 x2 y2 z2) = Vector3 (t / x2) (t / y2) (t / z2)

-- Left scalar division
(^/) :: Vector3 Float -> Float -> Vector3 Float
(Vector3 x2 y2 z2) ^/ t = Vector3 (x2 / t) (y2 / t) (z2 / t)


dot :: Vec3 -> Vec3 -> Float
dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2 + y1 * y2 + z1 * z2)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
  Vector3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

length :: Vec3 -> Float
length (Vector3 x y z) = sqrt $ x*x + y*y + z*z

lengthSqr :: Vec3 -> Float
lengthSqr (Vector3 x y z) = x*x + y*y + z*z

norm :: Vec3 -> Vec3
norm (Vector3 x y z) =
  let mag = sqrt $ x*x + y*y + z*z
  in Vector3 (x/mag) (y/mag) (z/mag)
