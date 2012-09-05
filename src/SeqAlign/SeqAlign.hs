-----------------------------------------------------------------------------
--
-- Module      :  SeqAlign
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

module SeqAlign (
    labSequenceAlignment, seqAlign
) where

import System.IO
import Array
import Garbage

labSequenceAlignment = seqAlign

seqAlign = do
    testFileIO

--inputFilePath = "d:/Projects/Haskell/Lab1/src/seqInput"
--outputFilePath = "d:/Projects/Haskell/Lab1/src/seqOutput"
inputFilePath = "data/sequence.in"
outputFilePath = "data/sequence.out"

testFileIO = do
    ifHandle <- openFile inputFilePath ReadMode
    s <- hGetContents ifHandle
    randA <- getNuclSeqIO 200
    randB <- getNuclSeqIO 100
    let
        a:b:_ = lines s
        --a:b:_ = [randA, randB]
        arr   = nwTable a b
        seqs  = getSeqs a b arr
    --printRows (getArrayByRows arr)
    --printRows seqs
    ofHandle <- openFile outputFilePath WriteMode
    printRows seqs
    hPrint' ofHandle seqs
    hFlush ofHandle

hPrint' hOut [] = return ()
hPrint' hOut (x:xs) = do
    hPrint hOut x
    hPrint' hOut xs
-----------------------------------------------------
gap = -2
eC  = '_'

diff :: Eq a => a -> a -> Int
diff x y
    | x == y    = 1
    | otherwise = 0

nwTable :: String -> String -> Array (Int, Int) Int
nwTable a b = table where
    table   = array ((0,0), (nA, nB)) (baseA ++ baseB ++ other)
    baseA   = [((i, 0), gap * i)    | i <- [0..nA]]
    baseB   = [((0, j), gap * j)    | j <- [0..nB]]
    other   = [((i, j), best i j)   | i <- [1..nA], j <- [1..nB]]
    best i j = maximum[
        table ! (i-1, j-1) + diff (a !! (i-1)) (b !! (j-1)),
        table ! (i-1, j) + gap,
        table ! (i, j-1) + gap]
    nA      = length a
    nB      = length b

nwTableL :: [String] -> Array (Int, Int) Int
nwTableL (x : y : xys) = nwTable x y

------------------------------------------------------
getSeqs a b arr = unzip' (getResult (reverse (getPath nA nB arr)) a b)
    where
        nA = length a
        nB = length b


getPath 0 0 arr = []
getPath i 0 arr = 1     : getPath (i-1) 0 arr
getPath 0 j arr = (-1)  : getPath 0 (j-1) arr
getPath i j arr
    | arr ! (i, j) == arr ! (i-1, j) + gap = 1      : getPath (i-1) j arr
    | arr ! (i, j) == arr ! (i, j-1) + gap = (-1)   : getPath i (j-1) arr
    | otherwise = 0 : getPath (i-1) (j-1) arr

getResult [] [] []              = []
getResult (x:xs) (a:aa) []      = (a, eC) : (getResult xs aa [])
getResult (x:xs) [] (b:bb)      = (eC, b) : (getResult xs [] bb)
getResult (x:xs) (a:aa) (b:bb)
    | x == 0    = (a, b)    : (getResult xs aa bb)
    | x > 0     = (a, eC)   : (getResult xs aa (b:bb))
    | x < 0     = (eC, b)   : (getResult xs (a:aa) bb)

unzip' a = [(fst x), (snd x)]
    where x = unzip a

-----------------------------------------------------
