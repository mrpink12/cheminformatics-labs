import System.Environment
import Control.Monad
import LabsIntro
import SeqAlign
import Mol2Visualizer
import Mol2Parser
import Superimposition
 
-- | 'main' runs the main program
main :: IO ()
main = do
        (lab:args) <- getArgs
        case lab of
                "intro" -> testLabsIntro
                "seqal" -> testSequenceAlignment
                "mol2"  -> testMol2Parser args
                "vis"   -> testVisualizer args

testLabsIntro = do
        labsIntro
        
testSequenceAlignment = do
        labSequenceAlignment

testMol2Parser (input:output:_) = do
        mols <- getMol2 input
        putMol2 output mols
        
testVisualizer (input:_) = do
        mols <- getMol2 input
        renderMolecules mols
        
testSuperimposition (in1:in2:out:_) = do
        (m1:_) <- getMol2 in1
        (m2:_) <- getMol2 in2
        let
                m = superimpose m1 m2
        putMol2 out (m:[])