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

testGlut = do
  getArgsAndInitialize

  initialDisplayMode $= [WithDepthBuffer,DoubleBuffered]
  createAWindow "test"
  depthFunc $= Just Less
  lighting $= Enabled
  position (Light 0) $= Vertex4 0.5 0.5 (-5) 1
  light (Light 0) $= Enabled
  mainLoop

createAWindow windowName = do
  createWindow windowName
  displayCallback $= display
  idleCallback $= Just display

display = do
  clear [ColorBuffer, DepthBuffer]
  currentColor $= Color4 1 1 0 1
  loadIdentity
  translate $ Vector3 0.5 0 (0::GLfloat)
  renderObject Solid $ (Sphere' 0.8 20 10)
  loadIdentity
  translate $ Vector3 (-0.5) 0 (0::GLfloat)
  renderObject Solid $ (Sphere' 0.8 20 15)
  swapBuffers

drawSphere rad color = do
  currentColor $= color
  renderObject Solid $ (Sphere' rad 20 10)

