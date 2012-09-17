module PointsForRendering (
        displayPoints, makeVertexes
) where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL


displayPoints points primitiveShape = do
        renderPrimitive primitiveShape $ makeVertexes points
        flush

makeVertexes = mapM_ (\(x,y,z) -> vertex $ Vertex3 (x::GLfloat) y z)
