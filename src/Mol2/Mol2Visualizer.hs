{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Mol2Visualizer (
    renderMolecule
) where

import GHC.Float
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import OrbitPointOfView
import Data.IORef
import System.IO
import Mol2Parser
import Shapes

import Garbage
import Data.List
import Geometry

        
renderMolecule m @ (Molecule (Header molName _ _) atoms bonds) = do
        initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
        (progName,_) <- getArgsAndInitialize
        createWindow molName
        depthFunc $= Just Less

        pPos <- newIORef (0::Int, 90::Int, 2.0)
        vType <- newIORef (2::Int)
        displayCallback $= display vType pPos atoms bonds
        keyboardMouseCallback $= Just (keyboard vType pPos)

        reshapeCallback $= Just reshape
        mainLoop
       
display vType pPos atoms bonds = do
        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer, DepthBuffer]
        loadIdentity
        setPointOfView pPos        
        x <- get vType
        if x == 1 
                then do mapM_ (\atom -> renderAtom atom) atoms
                else do mapM_ (\bond -> renderBond bond atoms) bonds
        swapBuffers

keyboard vType pPos (Char '1') _ _ _ = do
        vType $= 1        
keyboard vType pPos (Char '2') _ _ _ = do
        vType $= 2
keyboard vType pPos c _ _ _ = keyForPos pPos c 10

renderAtom a @ (Atom atomId atomName atomType point@(Point3 px py pz) charge) = preservingMatrix $ do
        currentColor $= atomColor4 atomType
        let rad = vdwRadius atomType
        translate $ Vector3 px py pz
--        renderObject Wireframe $ Sphere' rad 30 30
        renderObject Solid $ Sphere' rad 30 30
                
renderBond b @ (Bond bondId idX idY bondType) atoms = preservingMatrix $ do
        let
                a1 @ (Atom _ _ _ p1 _) = findAtom' idX atoms
                a2 @ (Atom _ _ _ p2 _) = findAtom' idY atoms
                direction = p2 <-> p1
        renderBondPart a1 direction
        renderBondPart a2 $ pNeg direction

renderBondPart a@(Atom _ _ atomType p@(Point3 x y z) _) direction = preservingMatrix $ do
        currentColor $= atomColor4 atomType
        let
                (alpha, beta, r) = toRadial direction
                rad = 0.3
        translate $ Vector3 x y z
        rotate (radianToDegrees alpha) $ toVector3 vecY
        rotate (radianToDegrees beta) $ toVector3 vecX
        renderCylinder (double2Float rad) 30 $ r / 2
        renderObject Solid $ Sphere' rad 30 30        
                

findAtom' id atoms = atoms !! (id - 1)

toVector3 (Point3 x y z) = Vector3 x y z

vdwRadius atomType
    | atomType == "H"  = 1.20
    | atomType == "F"  = 1.47
    | atomType == "Cl" = 1.75
    | atomType == "Br" = 1.85
    | atomType == "I"  = 1.98
    | isPrefixOf "C." atomType = 1.70
    | isPrefixOf "N." atomType = 1.55
    | isPrefixOf "O." atomType = 1.52
    | isPrefixOf "S." atomType = 1.80
    | isPrefixOf "P." atomType = 1.80
    | otherwise = 2.0

atomColor4 atomType
    | atomType == "H"  = Color4 1 1 1 1 -- white
    | atomType == "F"  = Color4 0 1 0 1 -- green
    | atomType == "Cl" = Color4 0 1 0 1 -- green
    | atomType == "Br" = Color4 0 1 0 1 -- green
    | atomType == "I"  = Color4 0 1 0 1 -- green
    | isPrefixOf "C." atomType = Color4 0.5 0.5 0.5 1 -- gray
    | isPrefixOf "N." atomType = Color4 0 0 1 1 -- blue
    | isPrefixOf "O." atomType = Color4 1 0 0 1 -- red
    | isPrefixOf "S." atomType = Color4 1 1 0 1 -- yellow
    | isPrefixOf "P." atomType = Color4 1 0 1 1 -- magenta
    | otherwise = Color4 1 0 1 1 -- magenta
