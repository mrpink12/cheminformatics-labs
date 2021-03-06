-----------------------------------------------------------------------------
--
-- Module      :  LabsIntro
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
module LabsIntro (
    labsIntro,
    genListIO
) where

import System.Random
import Data.List (sort)

labsIntro = do
    solve1
    solve2
    solve3
    solve4
    solve5


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

-------------------Problem 1. Print list-------------------------
solve1 = do
    gen <- newStdGen
    let arr = genList gen 4 100
    print arr

-------------------Problem 2. Find median------------------------
solve2 = do
    gen <- newStdGen
    let arr = sort (genList gen 12 100)
        m   = median (arr)
    print arr
    print m

median :: [Int] -> Float
median []       = error "empty array"
median [x]      = fromIntegral x
median [x,y]    = (fromIntegral(x + y)) / 2
median (x:xs)   = median (init xs)

------------------Problem 3. Gold ratio--------------------------
solve3 = do
    print (extByGoldRatio 1 2 testFunc (<))
    print (extByGoldRatio1 1 2 testFunc (<))

testFunc x = -(exp (-x)) * (log x)
testFunc1 x = -(x**2)
r = (1 + 5**0.5) / 2
eps = 5e-6

extByGoldRatio :: Float -> Float -> (Float -> Float) -> (Float -> Float -> Bool) -> Float
extByGoldRatio a b f comparator
    | abs(b - a) < eps          = (a + b) / 2
    | comparator (f x0) (f x1)  = extByGoldRatio a x1 f comparator
    | otherwise                 = extByGoldRatio x0 b f comparator
    where
        dx  = (b - a) / r
        x0  = b - dx
        x1  = a + dx

extByGoldRatio1 :: Float -> Float -> (Float -> Float) -> (Float -> Float -> Bool) -> Float
extByGoldRatio1 a b f comparator
    | abs(b - a) < eps          = (a + b) / 2
    | comparator (f a) (f b)    = extByGoldRatio a x0 f comparator
    | otherwise                 = extByGoldRatio x1 b f comparator
    where
        dx  = (b - a) / r
        x0  = b - dx
        x1  = a + dx

------------------Problem 4. Local extremum with steps-----------------------
solve4 = do
    extremum 1 2 testFunc (<)

printStep :: Float -> Float -> IO ()
printStep a b = do
    let g :: Show a => a -> a -> ShowS
        g x y = ('[':) . shows x . (',':) . shows y.(']':)
        s = (g a b) ""
    putStrLn s

extremum :: Float -> Float -> (Float -> Float) -> (Float -> Float -> Bool) -> IO ()
extremum a b f comp
    | abs(b - a) < eps  = do
        let x = (a + b) / 2
        putStr "result = "
        print [x, f(x)]
    | comp (f a) (f b)  = do
        printStep a b
        extremum a x0 f comp
    | otherwise         = do
        printStep a b
        extremum x1 b f comp
    where
        x0  = b - (b - a) / r
        x1  = a + (b - a) / r

------------------Problem 5. Generation-----------------------
solve5 = do
    print (genNumbers 9 8 10)

genNumbers :: (Ord a, Num a) => a -> a -> a -> [a]
genNumbers x y d
    | x > y     = []
    | x <= y    = x:(genNumbers (x+d) y d)
