import System.Environment
import Control.Monad
import LabsIntro
import SeqAlign
import Mol2Visualizer
 
-- | 'main' runs the main program
main :: IO ()
main = do
        (fileName: _) <- getArgs
        mol2Main fileName
--    labsIntro
--    labSequenceAlignment
