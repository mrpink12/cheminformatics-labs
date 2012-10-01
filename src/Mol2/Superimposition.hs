module Superimposition
where


import Mol2Visualizer
import Mol2Parser
import Geometry

superimpose m1 m2 (state:_) = do
        case state of
                "1" ->    do renderMolecules (m1:m2:[])
                "2" ->    do centerMols (m1:m2:[]) >>= renderMolecules
                "3" ->    do centerMols (m1:m2:[]) >>= fitMols >>= renderMolecules

centerMols mols = do        
        return $ map (\(Molecule h atoms bonds) -> (Molecule h (center atoms) bonds)) mols

center atoms = map (\(Atom id name atype p charge) -> (Atom id name atype (p <-> c) charge)) atoms
        where
                c = centroid testWeightFunc atoms --vdwRadius atoms

rmsd ((Molecule _ atoms1 _):(Molecule _ atoms2 _):_) = rmsd' (map filterP atoms1) (map filterP atoms2)
                
rmsd' points1 points2 = sqrt $ s / len
        where
                len = fromIntegral $ length points1
                distance (p1, p2) = pDist2 $ p1 <-> p2
                s = foldr (+) 0 $ map distance $ zip points1 points2   

getPointsFromMol (Molecule _ atoms _) = map filterP atoms

filterP (Atom _ _ _ p _) = p
replacePoint (Atom id name atype p charge) p1 = (Atom id name atype p1 charge)
replacePointInMol (Molecule h atoms bonds) points = (Molecule h atoms' bonds)
        where
                atoms' = map (\(a, p) -> replacePoint a p) $ zip atoms points

fitMols mols@(m:ms) = do
        let
                (a:b:_)         = map getPointsFromMol mols
                d               = rmsd' a b
                h               = pi
                (a', _, d')     = hookJeves rmsd' a b (h, h, h) d
                mols'           = ((replacePointInMol m a'):ms)
        print d
        print d'
        return mols'
                
--------------HookJeves--------------------
hookJeves f a b (h1, h2, h3) d
        | abs (d' - d) < 0.1 && maximum(h1:h2:h3:[]) < 0.01     = (a', b, d')
        | otherwise                                             = hookJeves f a' b (h1', h2', h3') d' 
        where
                (a', d', (h1', h2', h3')) = tryHJ f a b (h1, h2, h3) d
                
tryHJ f a b (h1, h2, h3) d
        | h1 == maximum (h1:h2:h3:[])   = nextStepHJ f a b (h1, h2, h3) Ox d
        | h2 == maximum (h1:h2:h3:[])   = nextStepHJ f a b (h1, h2, h3) Oy d
        | otherwise                     = nextStepHJ f a b (h1, h2, h3) Oz d
        
nextStepHJ f a b (h1, h2, h3) axys d 
        | axys == Ox    = (a1, d1, (h1', h2, h3))
        | axys == Oy    = (a2, d2, (h1, h2', h3))
        | otherwise     = (a3, d3, (h1, h2, h3'))
        where
                (a1, h1', d1) = minimizeHJ f a b h1 Ox d
                (a2, h2', d2) = minimizeHJ f a b h2 Oy d
                (a3, h3', d3) = minimizeHJ f a b h3 Oz d 
        
minimizeHJ f a b h axys d 
        | d1 < d && d1 <= d2    = (a1, h, d1)
        | d2 < d && d2 <= d1    = (a2, h, d2)      
        | otherwise             = (a, h / 2, d)
        where                
                (a1, d1) = testRotate f a b h axys d
                (a2, d2) = testRotate f a b (-h) axys d
        
testRotate f a b h axys d 
        | d' < d        = (a', d')
        | otherwise     = (a, d)
        where
                a'      = rotate a h axys
                d'      = f a' b
                
rotate arr h axys = map (\p -> pRotate p axys h) arr

----------------CENTROID-------------------
centroid weightFunc atoms = centroid' weightFunc atoms c 0 where
   c = Point3 0.0 0.0 0.0

centroid' wf [] c w = c
centroid' wf (a@(Atom _ _ atype p _):atoms) c w = centroid' wf atoms nc nw where
    pw = wf atype
    nw = w + pw
    alpha = pw / nw
    nc = c <+> ((p <-> c) <<*> alpha)

testWeightFunc atype = 1.0