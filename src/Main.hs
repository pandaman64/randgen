{-# LANGUAGE RankNTypes #-}

import System.Random
import Data.Array.ST
import Data.STRef
import Control.Monad.Random
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Array

randomNumbersIO :: Int -> IO [Int]
randomNumbersIO n = do
    gen <- newStdGen
    return $ randomNumbers n gen

randomNumbers :: RandomGen g => Int -> g -> [Int]
randomNumbers n gen = elems $ runSTArray $ do
    v <- evalRandT (randomAssignN n) gen
    a <- v
    return a
    where
        randomAssignN n = do
            ops <- forM [0..n] $ randomAssign
            return $ foldl1 (flip (.)) ops (newArray_ (0,n))
        randomAssign n = do
            value <- getRandomR (0,n)
            return $ \ma -> do
                a <- ma
                if value == n then
                    writeArray a n n
                else do
                    v <- readArray a value
                    writeArray a n v 
                    writeArray a value n
                return a

main = do
    numbers <- randomNumbersIO 10
    putStrLn $ show numbers
