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
myprint (as,iter)= print $ "First frequency that appeared twice: " ++ show (frequencyRT as) ++ " found after " ++ show iter ++ " iterations."

searchF fs st iter = 
    let appStResult = runST $ do
        appSt <- newSTRef st
        compute fs appSt in
    if Data.Maybe.isJust $ frequencyRT appStResult
    then (appStResult, iter)
    else searchF fs appStResult (iter+1)

main :: IO ()
main = do 
    input <- getInput
    let fs = toInts input
    myprint $ searchF fs initState 1
