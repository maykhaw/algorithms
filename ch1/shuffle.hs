import Data.Array
import Data.Array.ST
import Control.Monad.ST
import Control.Monad
import System.Random

fisherYates :: (RandomGen g,Ix ix, Random ix) => g -> Array ix e -> Array ix e
fisherYates gen a' = runSTArray $ do
  a <- thaw a'
  (bot,top) <- getBounds a
  foldM (\g i -> do
    ai <- readArray a i
    let (j,g') = randomR (bot,i) g
    aj <- readArray a j
    writeArray a i aj
    writeArray a j ai
    return g') gen (range (bot,top))    
  return a
