-----------------------------------------------------------------------------
--
-- Module      :  TestGlut
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

module TestGlut (
    testGlut
) where

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL

testGlut = mainFor Triangles
    
mainFor primitiveShape = renderInWindow someSquares
    
renderInWindow displayFunction = do
    (progName, _) <- getArgsAndInitialize
    createWindow progName
    displayCallback $= displayFunction
    mainLoop

-------------------------------------------------------------
myRect width height =
    displayPoints [(w,h,0),(w,-h,0),(-w,-h,0),(-w,h,0)] Quads
    where
        w = width/2
        h = height/2
        
square width = myRect width width

rotatedSquare alpha width = do
    rotate alpha $ Vector3 0 0 (1 :: GLfloat)
    square width
    
displayAt x y displayMe = do
    translate $ Vector3 x y (0::GLfloat)
    displayMe
    loadIdentity
    
someSquares = do
    clearColor $= Color4 1 1 1 1
    clear [ColorBuffer]
    
    currentColor $= Color4 1 0 0 1
    displayAt 0.5 0.3 $ rotatedSquare 15 0.12

    currentColor $= Color4 0 1 0 1
    displayAt (-0.5) 0.3 $ rotatedSquare 25 0.5
    
    currentColor $= Color4 0 0 1 1
    displayAt (-1) (-1) $ rotatedSquare 4 0.75
    
    currentColor $= Color4 0 1 1 1
    displayAt 0.7 0.7 $ rotatedSquare 40 0.3
    
--------------------------------------------------------------
ringAt x y innerRadius outerRadius = do
    translate $ Vector3 x y (0::GLfloat)
    ring innerRadius outerRadius
    
someRings = do
    clearColor $= Color4 0 0 0 1
    clear [ColorBuffer]
    
    loadIdentity
    currentColor $= Color4 1 0 0 1
    ringAt 0.5 0.3 0.1 0.12

    loadIdentity
    currentColor $= Color4 0 1 0 1
    ringAt (-0.5) 0.3 0.3 0.5
    
    loadIdentity
    currentColor $= Color4 0 0 1 1
    ringAt (-1) (-1) 0.7 0.12
    
    loadIdentity    
    currentColor $= Color4 0 1 1 1
    ringAt 0.7 0.7 0.2 0.3

--------------------------------------------------------------
ringPoints innerRadius outerRadius
    = concat $ map (\ (x, y) -> [x, y]) (points ++ [p])
    where
        innerPoints = circle innerRadius
        outerPoints = circle outerRadius
        points @ (p : _) = zip innerPoints outerPoints
        
ring innerRadius outerRadius = displayPoints  (ringPoints innerRadius outerRadius) QuadStrip

--------------------------------------------------------------
renderCircleApprox r n = displayPoints (circlePoints r n) LineLoop
renderCircle r = displayPoints (circle r) LineLoop
fillCircle r = displayPoints (circle r) Polygon

circle radius = circlePoints radius 100

circlePoints radius number = 
    [let 
        alpha = 2 * pi * i / number
        x = radius * sin(alpha)
        y = radius * cos(alpha)
    in (x::GLfloat, y::GLfloat, 0) | i <- [1, 2 .. number]]

----------------------------------------------------------------
displayPoints points primitiveShape = do
    renderAs primitiveShape points
    flush
    
renderAs figure ps = renderPrimitive figure$makeVertexes ps
makeVertexes = mapM_ (\ (x, y, z) -> vertex $ Vertex3 x y z)
renderPoints = renderAs Points


