-----------------------------------------------------------------------------
--
-- Module      :  Geometry
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Geometry where

import Mol2Parser;

data Axys = Ox | Oy | Oz deriving Eq

----------------TRANSLATE------------------
translateMol (Molecule h atoms bonds) p =
    Molecule h (translateAtoms atoms p) bonds

translateAtoms [] _ = []
translateAtoms (a:as) v = translateAtom a v : translateAtoms as v

translateAtom (Atom id n t p ch) v =
    Atom id n t (p <+> v) ch

-----------------ROTATE--------------------
rotateMol (Molecule h atoms bonds) axys angle = Molecule h (rotateAtoms atoms axys angle) bonds

rotateAtoms [] _ _ = []
rotateAtoms (a:as) axys angle =
    rotateAtom a axys angle : rotateAtoms as axys angle

rotateAtom (Atom id n t p ch) axys angle = Atom id n t p' ch where
    p' = pRotate p axys angle

pRotate (Point3 x y z) Ox angle = (Point3 x y' z') where
    y' =  y * cos angle + z * sin angle
    z' = -y * sin angle + z * cos angle

pRotate (Point3 x y z) Oy angle = (Point3 x' y z') where
    x' =  x * cos angle - z * sin angle
    z' =  x * sin angle - z * cos angle

pRotate (Point3 x y z) Oz angle = (Point3 x' y' z) where
    x' =  x * cos angle + y * sin angle
    y' = -x * sin angle + y * cos angle

------------------Point3------------------
instance Eq Point3 where
    (Point3 px py pz) == (Point3 px' py' pz') =
        (px == px') && (py == py') && (pz == pz')

(Point3 px py pz) <+> (Point3 px' py' pz') =
    Point3 (px + px') (py + py') (pz + pz')

(Point3 px py pz) <<*> x = Point3 (px * x) (py * x) (pz * x)
x <*>> p @ (Point3 px py pz) = p <<*> x

(Point3 px py pz) <*> (Point3 px' py' pz') = px * px' + py * py' + pz * pz'

(Point3 px py pz) <^> (Point3 px' py' pz') = Point3 x y z where
    x = pz * py' - py * pz'
    y = px * pz' - pz * px'
    z = py * px' - px * py'

pDist2 (Point3 x y z) = x * x + y * y + z * z
pDist p = sqrt $ pDist2 p

pNeg (Point3 x y z) = Point3 (-x) (-y) (-z)

p <-> p' = p <+> (pNeg p')
        
toRadial p @ (Point3 x y z) = (alpha, (-beta), r)
        where
                r = pDist p
                alpha = atan2 x z
                t = sqrt $ r * r - y * y
                beta = atan2 y t
                
radianToDegrees :: RealFrac a => a -> a
radianToDegrees x 
        | x < 0         = x * 57.295779513082322 + 360
        | x >= 0        = x * 57.295779513082322
                

pZero = Point3 0.0 0.0 0.0
vecX = Point3 1.0 0.0 0.0
vecY = Point3 0.0 1.0 0.0
vecZ = Point3 0.0 0.0 1.0
