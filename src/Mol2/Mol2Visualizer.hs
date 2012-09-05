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

import System.IO
import Mol2Parser
import Garbage
--import Geometry
import TestGlut

tMol2File = "data/aspirin.mol2"
outputMol2File = "data/aspirin_out.mol2"

mol2Main = do
    ifHandle <- openFile tMol2File ReadMode
    cont <- hGetContents ifHandle
    let
        mols = readMol2 cont
    --putMol2 outputMol2File mols
    printRows mols
    return ()
    --testGlut
