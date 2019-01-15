
module Ex1 where 
import Data.List

getInput :: IO String
getInput = readFile "ex1-input2.txt"

elem1or0 v d = if v `elem` d then 1 else 0

countSubgroupLetters :: [String] -> [[Int]]
countSubgroupLetters = map (countSubgroup . sortAndGroup)
    where 
        sortAndGroup = group . sort
        countSubgroup = map length

count2and3 :: [[Int]] -> (Int,Int)
count2and3 d = (count2 d, count3 d)
        where 
            count2 d = sum $ map (elem1or0 2) d
            count3 d = sum $ map (elem1or0 3) d

prod :: (Int,Int) -> Int
prod (a,b) = a * b

main = do
    input <- getInput
    print $ (prod . count2and3 . countSubgroupLetters) $ lines input