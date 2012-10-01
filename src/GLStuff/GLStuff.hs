-----------------------------------------------------------------------------
--
-- Module      :  GLStuff
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

module GLStuff (
    testGlut
) where

import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL

testGlut = mainFor Triangles
    
mainFor primitiveShape = renderInWindow someRings
    
renderInWindow displayFunction = do
    (progName, _) <- getArgsAndInitialize
    createWindow progName
    displayCallback $= displayFunction
    mainLoop

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


