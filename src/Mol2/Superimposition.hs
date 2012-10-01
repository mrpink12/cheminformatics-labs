module Superimposition
where


import Mol2Visualizer
import Mol2Parser
import Geometry

superimpose m1 m2 = m1
-- getRMSD file1 file2 = do
        -- m1 = readMol2

        
----------------CENTROID-------------------
-- centroid weightFunc pSet = centroid' weightFunc pSet c 0 where
   -- c = Point3 0.0 0.0 0.0

-- centroid' wf [] c w = c
-- centroid' wf (p:ps) c w = centroid' wf ps nc (w + 1) where
    -- pw = wf p
    -- alpha = w / (w + pw)
    -- nc = c <+> ((p <-> c) <<*> alpha)

-- weight::String->Float
-- weight atomName = 1.0