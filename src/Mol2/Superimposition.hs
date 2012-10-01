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

rmsd ((Molecule _ atoms1 _):(Molecule _ atoms2 _):_) = sqrt $ s / len
        where
                len = fromIntegral $ length atoms1
                filter (Atom _ _ _ p _) = p
                pairs = zip (map filter atoms1) (map filter atoms2)
                distance (p1, p2) = pDist2 $ p1 <-> p2
                s = foldr (+) 0 $ map distance pairs       

fitMols mols = do
        print $ rmsd mols
        return mols
                
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