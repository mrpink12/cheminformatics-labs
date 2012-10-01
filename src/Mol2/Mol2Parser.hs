-----------------------------------------------------------------------------
--
-- Module      :  Mol2Parser
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

module Mol2Parser (
    Point3(..), Atom(..),
    Bond(..), Molecule(..),
    Header(..),
    readMol2, getMol2, writeMol2, putMol2,
    vdwRadius
) where

import System.IO
import Data.List

directory = "data/"
extension = ".mol2"
inputFile = "data/histidine.mol2"
--outputFile = "data/aspirin_out.mol2"

sTest = "a.ar 1.335 -1.323 D D D A B C D"

_parse' s = ("", s) : [x | (_, t) <- _parse' s, x <- lex t]
_parse = tail . (map fst ) . (takeWhile (("", "") /= )) . _parse'

data Point3 = Point3 {px, py, pz :: Float} deriving (Show)
data Header = Header {molName, molSize, chargeType :: String} deriving Show
data Atom = Atom {atomId :: Int, atomName, atomType :: String, point :: Point3, charge :: Float} deriving Show
data Bond = Bond {bondId, idX, idY :: Int, bondType :: String} deriving Show
data Molecule = Molecule {molHeader :: Header, atoms :: [Atom], bonds :: [Bond]} deriving Show

--WRITE MOLECULES TO FILE OR STRING
putMol2 fileName mols = do
    ofHandle <- openFile (directory ++ fileName ++ extension) WriteMode
    mapM_ (\m -> mapM_ (hPutStrLn ofHandle) (writeMol2 m)) mols
    --hPrint' ofHandle mols
    hFlush ofHandle
    hClose ofHandle

-- hPrint' hOut [] = return ()
-- hPrint' hOut (m:ms) = do
    -- sequence $ map (hPutStrLn hOut) (writeMol2 m)
    -- hPrint' hOut ms

writeMol2 (m @ (Molecule h as bs)) = map ($ []) mol where
    mol = (showsHeader m) ++ (showsAtoms as) ++ (showsBonds bs)

showsHeader (Molecule (Header molName molSize chargeType) atoms bonds) =
    [header, showString molName, sizes, showString molSize, showString chargeType] where
        header  = (showString "@<TRIPOS>MOLECULE")
        sizes   = (shows $ length atoms).(' ':).(shows $ length bonds).(showString " 0 0 0")

showsAtoms as = (showString "@<TRIPOS>ATOM") : (showsAtomsBody as)
showsAtomsBody ((Atom atomId atomName atomType point charge) : as) =
    a : showsAtomsBody as where
        a = (shows atomId).(' ':).(showString atomName).(' ':).(showsPoint3 point).(' ':).(showString atomType).(' ':).(shows charge)
showsAtomsBody [] = []

showsBonds bs = (showString "@<TRIPOS>BOND") : (showsBondsBody bs)
showsBondsBody ((Bond bondId idX idY bondType) : bs) =
    b : showsBondsBody bs where
        b = (shows bondId).(' ':).(shows idX).(' ':).(shows idY).(' ':).(showString bondType)
showsBondsBody [] = []

showsPoint3 (Point3 px py pz) = (shows px).(' ':).(shows py).(' ':).(shows pz)

--READ MOLECULES FROM FILE OR STRING
getMol2 fileName = do 
        iFileHandle <- openFile (directory ++ fileName ++ extension) ReadMode
        cont <- hGetContents iFileHandle
        return $ readMol2 cont

readMol2 s = map buildMolecule str where
    x = lines s
    str = splitToBlocks (filterLines x 0) ([], [], [])

buildMolecule (h, a, b) = Molecule molHeader atoms bonds where
    molHeader   = parseHeader h
    atoms       = parseAtoms a
    bonds       = parseBonds b

--parse Molecule inner structures
parseHeader (molName:_:molSize:chargeType:_) = Header molName molSize chargeType

parseAtoms (a:as) = atom : parseAtoms as where
    (atomIdS:atomName:pxS:pyS:pzS:atomType:chargeS:_) = words a
    atomId  = readI atomIdS
    point   = Point3 (readF pxS) (readF pyS) (readF pzS)
    charge  = readF chargeS
    atom    = Atom atomId atomName atomType point charge
parseAtoms [] = []

parseBonds (b:bs) = bond : parseBonds bs where
    (bondIdS:idXS:idYS:bondType:_) = words b
    bondId  = readI bondIdS
    idX     = readI idXS
    idY     = readI idYS
    bond    = Bond bondId idX idY bondType
parseBonds [] = []

readI = read::String->Int
readF = read:: String->Float

--split to blocks
splitToBlocks ("@<TRIPOS>MOLECULE":ls) ([], [], []) =
    splitToBlocks nls (nh, [], []) where
        (nh, nls) = getBlock ls []
splitToBlocks ("@<TRIPOS>MOLECULE":ls) (h, a, b) =
    (h, a, b) : splitToBlocks nls (nh, [], []) where
        (nh, nls) = getBlock ls []
splitToBlocks ("@<TRIPOS>ATOM":ls) (h, a, b) =
    splitToBlocks nls (h, na++a, b) where
        (na, nls) = getBlock ls []
splitToBlocks ("@<TRIPOS>BOND":ls) (h, a, b) =
    splitToBlocks nls (h, a, nb++b) where
        (nb, nls) = getBlock ls []
splitToBlocks (l:ls) (h, a, b) = splitToBlocks ls (h, a, b)
splitToBlocks [] ([], [], []) = []
splitToBlocks [] (h, a, b) = (h, a, b):[]

--parse block from "@%" to "@%"
getBlock ((s @ ('@':_)) : ls) x = (reverse x, s:ls)
getBlock (s:ls) x = getBlock ls (s:x)
getBlock [] x = (reverse x, [])

--filter mol2 file
filterLines ((s @ "@<TRIPOS>MOLECULE") : ls) _  = s : filterLines ls 0
filterLines ((s @ "@<TRIPOS>ATOM") : ls) _      = s : filterLines ls 0
filterLines ((s @ "@<TRIPOS>BOND") : ls) _      = s : filterLines ls 0
filterLines (('@' : _) : ls) _                  = filterLines ls 1
filterLines (('#' : _) : ls) x                  = filterLines ls x
filterLines ("" : ls) x                         = filterLines ls x
filterLines (l : ls) x
    | x == 0    = l : filterLines ls x
    | x == 1    = filterLines ls x
filterLines [] _ = []

----------------------------MOL2 Settings-------------------------

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