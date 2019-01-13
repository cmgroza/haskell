{-# LANGUAGE RecordWildCards #-}

module Ex1 where

import Control.Monad.ST
import Data.STRef
import Data.Maybe
import qualified Data.Map as Map

data AppState = AppState
    {
        frequencies :: Map.Map Integer Integer,
        frequencyRT :: Maybe Integer,
        currentF :: Integer
    }deriving (Show)

initState = AppState
    {
        frequencies = Map.fromList [(0,0)],
        frequencyRT = Nothing,
        currentF = 0
    }


isMember = Map.member

addElem v = Map.insert v v
    

getInput :: IO String
getInput = readFile "ex1-input.txt"

toInt :: String -> Integer
toInt ('-' : xs) = - toInt xs
toInt ('+' : xs) = toInt xs
toInt s = read s

toInts :: String -> [Integer]
toInts = map toInt . lines

updateAppState appSt@AppState{..} v
        | Data.Maybe.isJust frequencyRT = appSt
        | otherwise = let nf = v + currentF in
            if isMember nf frequencies
            then appSt {frequencyRT = Just nf}
            else appSt {frequencies = addElem nf frequencies, currentF = nf}

compute [] state = readSTRef state
compute (v:vs) state = do 
                appSt <- readSTRef state
                writeSTRef state (updateAppState appSt v)
                compute vs state

myprint :: (AppState, Integer) -> IO()
myprint = print

{-
main2 :: IO ()
main2 = do 
    input <- getInput
    myprint $ runST $ do
    appSt <- newSTRef initState
    compute (toInts input) appSt
-}


searchF ints st iter = 
    let result = runST $ do
        appSt <- newSTRef st
        compute ints appSt in
    if Data.Maybe.isJust $ frequencyRT result
    then (result, iter)
    else searchF ints result (iter+1)

main :: IO ()
main = do 
    input <- getInput
    let inputInts = toInts input
    myprint $ searchF inputInts initState 1


fibST :: Integer -> Integer
fibST n = 
    if n < 2
    then n
    else runST $ do
        x <- newSTRef 0
        y <- newSTRef 1
        fibST' n x y
    
        where fibST' 0 x _ = readSTRef x
              fibST' n x y = do
                    x' <- readSTRef x
                    y' <- readSTRef y
                    writeSTRef x y'
                    writeSTRef y $ x'+y'
                    fibST' (n-1) x y
