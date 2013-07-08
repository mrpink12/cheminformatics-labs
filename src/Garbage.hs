-----------------------------------------------------------------------------
--
-- Module      :  Garbage
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

module Garbage (
    genListIO, printRows, getArrayByRows,
    getNuclSeqIO, showsStrings
) where

import Array
import System.Random

-------------Generate random list---------------------
genListIO n m = do
    gen <- newStdGen
    return (genList gen n m)

genList gen n m = let (a, b) = genPreList gen n
                  in map (`mod` m) a

genPreList :: StdGen -> Int -> ([Int], StdGen)

genPreList gen 0 = ([], gen)
genPreList gen n =
    let (x, gen1)   = next gen
        (xs, gen2)  = genPreList gen1 (n - 1)
    in  (x:xs, gen2)

-------------------Lists IO---------------------------
printRows [] = do
    return ()
printRows (x : xs) = do
    print x
    printRows xs

--------------------Arrays----------------------------
getArrayByRows a = [[a ! (i, j) | j <- [0..nB]] | i <- [0..nA]]
    where
        nA = (fst.snd.bounds) a
        nB = (snd.snd.bounds) a

showsStrings (l:[])        = showString l
showsStrings (l:ls)        = (showString l).(' ':).(showsStrings ls)
        
-- showsList::Show a => [a] -> Shows a
-- showsList (l:ls)        = (shows l).(showsList ls)
-- showsList []            = shows []
------------------Nucleotides------------------------
getNuclSeqIO n = do
    arr <- genListIO n 4
    return (getNuclSeq arr)

getNuclSeq = (map getNucl)

getNucl 0 = 'A'
getNucl 1 = 'G'
getNucl 2 = 'C'
getNucl 3 = 'T'
getNucl x = error "hey!"
