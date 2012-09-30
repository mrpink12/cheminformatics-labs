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
    mol2Main
) where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import OrbitPointOfView
import Data.IORef
import System.IO
import Mol2Parser

import Garbage
import Data.List
--import Geometry

inputFile = "data/histidine.mol2"
--outputFile = "data/aspirin_out.mol2"

mol2Main = do 
        iFileHandle <- openFile inputFile ReadMode
        cont <- hGetContents iFileHandle
        let 
                (mol:mols) = readMol2 cont
        renderMolecule mol
        return ()
        
renderMolecule m @ (Molecule (Header molName _ _) atoms bonds) = do
        initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
        (progName,_) <- getArgsAndInitialize
        createWindow molName
        depthFunc $= Just Less

        pPos <- newIORef (0::Int, 90::Int, 2.0)
        displayCallback $= display pPos atoms
        keyboardMouseCallback $= Just (keyboard pPos)

        reshapeCallback $= Just reshape
        mainLoop

display pPos atoms = do
        clearColor $= Color4 0 0 0 1
        clear [ColorBuffer, DepthBuffer]
        loadIdentity
        setPointOfView pPos
        mapM_ (\atom -> renderAtom atom) atoms
        swapBuffers

keyboard pPos c _ _ _ = keyForPos pPos c

renderAtom a @ (Atom atomId atomName atomType point@(Point3 px py pz) charge) = preservingMatrix $ do
        translate $ Vector3 px py pz
        currentColor $= atomColor4 atomType
        let rad = vdwRadius atomType
--        renderObject Wireframe $ Sphere' rad 30 30
        renderObject Solid $ Sphere' rad 30 30
        
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
