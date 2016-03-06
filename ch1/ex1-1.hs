{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Debug.Trace
import Data.Array.MArray
import Data.Array.ST
import Data.Array
import Control.Monad
import Test.QuickCheck
import qualified Data.List as List

-- insertion sort in a mutable array

insertSort :: forall a. (Ord a) => Array Int a -> Array Int a
insertSort a' = runSTArray $ do
  a <- thaw a'
  (bot :: Int, top :: Int) <- getBounds a
  let 
     {- f j = if top < j then return () 
                       else do (j' :: a) <- readArray a j
                               i <- insertA j (j - 1)
                               f (j + 1) 
       insertA j i = do
        if (i > bot) then do
          i' <- readArray a i
          j' <- readArray a j
          if (i' < j') then do swap j i a
                               insertA j (i -1)
            else writeArray a i j' 
          else return () -}
--  f bot
  return a

-- search :: forall a. (Ord a) => Int -> Int -> a -> Array Int a -> Int
search bot top x array =
  let -- helper :: Int -> a -> Array Int a -> _
      helper ind a array =
        do elem <- readArray array ind
           return $ elem < a in
  do
    (b, t) <- getBounds array
    when (bot < b) $ error "search, too low"
    when (t < top) $ error "search, too high"
    isSmaller <- helper bot x array
    if isSmaller then
      if bot >= top then return $ bot + 1 -- ?
      else search (bot + 1) top x array
      else return bot

-- shift :: forall a. (Ord a) => Int -> Int -> Array Int a -> Array Int a
shift newbot top array = do
  (b, t) <- getBounds array
  when (newbot < b) $ error "search, too low"
  when (t < top) $ error "search, too high"

  oldTop <- readArray array top
  let shift1 i = do
        r <- readArray array i
        writeArray array (i+1) r
  mapM_ shift1 [top-1, top-2 .. newbot]
  writeArray array newbot oldTop
  
insertSort' :: forall a. (Ord a, Show a) => Array Int a -> Array Int a
insertSort' a' = runSTArray $ do
  a <- thaw a'
  (bot :: Int, top :: Int) <- getBounds a
  let helper j = do
        j' <- readArray a j
        toInsert <- search bot (j-1) j' a
        shift toInsert j a
  mapM helper [bot+1..top]
  return a

drive_shift l bot top = runSTArray $ do
  a <- thaw (makeListArray l)
  shift bot top a
  return a

drive_search = runSTArray $ do
  a <- thaw (makeListArray [3,1,1,6,5])
  s <- search 1 3 10 a
  return $ trace (show s) a

-- swaps elements at indices x and y in the array
swap x y array = do
  x' <- readArray array x
  y' <- readArray array y
  writeArray array x y' 
  writeArray array y x' 


makeListArray :: [a] -> Array Int a
makeListArray a = listArray (1, length a) a

runTest :: [Int] -> Property
runTest a = let a' = makeListArray a
                sortl = makeListArray $ List.sort a in
            sortl === insertSort' a'


insertSortList :: Ord a => [a] -> [a]
insertSortList l = foldr insert1 [] l

insert1 :: Ord a => a -> [a] -> [a]
insert1 a [] = [a]
insert1 a (x:xs) | a > x = x : insert1 a xs
                 | otherwise = a : x : xs  

insert1fold :: Ord a => a -> [a] -> [a]
insert1fold a l =
  foldl helper [] l
  where helper :: b -> a -> b 
