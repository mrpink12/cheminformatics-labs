module Shapes (
        myRect, square
) where

import Graphics.Rendering.OpenGL
import PointsForRendering

myRect width height = displayPoints [(w,h,0),(w,-h,0),(-w,-h,0),(-w,h,0)] Quads
        where
                w = width/2
                h = height/2

square width = myRect width width
