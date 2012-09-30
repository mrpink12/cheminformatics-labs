module Shapes (
        myRect, square, renderCylinder
) where

import Graphics.Rendering.OpenGL
import PointsForRendering

myRect width height = displayPoints [(w,h,0),(w,-h,0),(-w,-h,0),(-w,h,0)] Quads
        where
                w = width/2
                h = height/2

square width = myRect width width


renderCylinder r cnt height = displayPoints (cylinderPoints r cnt height) QuadStrip 

cylinderPoints :: GLfloat -> GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
cylinderPoints r cnt height = concat $ map (\ (x, y) -> [x, y]) (points ++ [p])
        where
                closestCircle = circlePoints r cnt 0
                farestCrcle = circlePoints r cnt height
                points @ (p : _) = zip closestCircle farestCrcle

circlePoints radius number z = 
        [let 
                alpha = 2 * pi * i / number
                x = radius * sin(alpha)
                y = radius * cos(alpha)
        in (x::GLfloat, y::GLfloat, z) | i <- [1, 2 .. number]]         
    