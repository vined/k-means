module Main where

import System.Environment (getArgs)
import System.Random (getStdGen, randomRs)
import System.IO (FilePath, readFile)
import Data.List (map)


splitOn :: Char -> String -> [String]
splitOn delim str =
    if length str < 1
        then []
        else (\(a, b) -> if length b < 1 then [str] else [a] ++ (splitOn delim (tail b))) (break (== delim) str)


parseLine :: String -> [String]
parseLine line = splitOn ',' line


readData :: FilePath -> IO [[String]]
readData filePath = do
    content <- readFile filePath
    return $ map parseLine $ lines content


main :: IO ()
main = do
    (fileName:clustersCntStr:ignoredColumnsStrs) <- getArgs
    putStrLn ""
    putStrLn "=== K-Means ==="

    putStrLn ""
    putStrLn "-- Reading data --"
    putStrLn $ "File: " ++ fileName
    d <- readData fileName
    putStrLn $ "Dataset size: " ++ (show $ length d)
    putStrLn $ "Columns count: " ++ (show $ length $ head d)

    putStrLn ""
    putStrLn "-- Running K-Means --"

    let clustersCnt = read clustersCntStr :: Int
    let ignoredColumns = map read ignoredColumnsStrs :: [Int]
    putStrLn $ "Clusters count: " ++ (show clustersCnt)
    putStrLn $ "Ignored columns: " ++ (foldl (\acc c -> acc ++ (show c) ++ ", ") "" ignoredColumns)



    let dataSize = 200
    g <- getStdGen
    let initialCenters = take clustersCnt (randomRs (0, 200) g) :: [Int]

    putStrLn $ "Initial centers: " ++ (foldl (\acc c -> acc ++ (show c) ++ ", ") "" initialCenters)
