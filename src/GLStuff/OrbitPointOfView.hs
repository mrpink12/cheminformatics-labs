module OrbitPointOfView where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT

import Data.IORef

setPointOfView pPos = do
    (alpha, beta, r) <- get pPos
    let
        (x, y, z) = calculatePointOfView alpha beta r
        (x2, y2, z2) = calculatePointOfView ((alpha + 90) `mod` 360) beta r
    lookAt (Vertex3 x y z) (Vertex3 0 0 0) (Vector3 x2 y2 z2)

calculatePointOfView alp bet r =
    let 
        alpha = fromDegree alp
        beta = fromDegree bet
        y = r * sin alpha
        u = r * cos alpha
        x = u * cos beta
        z = u * sin beta
    in (x, y, z)

keyForPos pPos (Char '+') rate = modPos pPos (id, id, \x -> x - fromIntegral(rate) * 0.1)
keyForPos pPos (Char '-') rate = modPos pPos (id, id, (+) (fromIntegral(rate) * 0.1))
keyForPos pPos (SpecialKey KeyLeft) rate = modPos pPos (id, (+) (360 - rate), id)
keyForPos pPos (SpecialKey KeyRight) rate = modPos pPos (id, (+) rate, id)
keyForPos pPos (SpecialKey KeyUp) rate = modPos pPos ((+) (360 - rate), id, id)
keyForPos pPos (SpecialKey KeyDown) rate = modPos pPos ((+) rate, id, id)
keyForPos _ _ _ = return ()

modPos pPos (ffst,fsnd,ftrd) = do
    (alpha,beta,r) <- get pPos
    pPos $= (ffst alpha `mod` 360,fsnd beta `mod` 360,ftrd r)
    postRedisplay Nothing

reshape screenSize @ (Size w h) = do
    viewport $= ((Position 0 0), screenSize)
    matrixMode $= Projection
    loadIdentity
    let 
        near = 0.001
        far = 40
        ang = fromDegree 45
        top = near / ( cos(ang) / sin(ang) )
        aspect = fromIntegral(w)/fromIntegral(h)
        right = top*aspect
    frustum (-right) right (-top) top near far
    matrixMode $= Modelview 0
    
fromDegree alpha = fromIntegral alpha * 2 * pi / fromIntegral 360
