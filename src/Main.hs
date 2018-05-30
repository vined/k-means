module Main where

import System.Environment (getArgs)
import System.Random (getStdGen, randomRs)
import System.IO (FilePath, readFile)
import Data.List (find, groupBy, map, sortOn, zip)


type Cls = Int


-- Utils --

getN :: [a] -> Int
getN a = (length a) - 1


roundTo :: Int -> Double -> Double
roundTo i d =
    let
        pow = 10.0 ^ i
    in
        (fromIntegral $ round (pow * d)) / pow


-- IO --

splitOn :: Char -> String -> [String]
splitOn delim str =
    if length str < 1
        then []
        else (\(a, b) -> if length b < 1 then [str] else [a] ++ (splitOn delim (tail b))) (break (== delim) str)


splitLine :: String -> [String]
splitLine = splitOn ','


-- getColMean :: [[Double]] -> Int -> Double
-- getColMean rows idx


-- replaceEmptyWithMean rows


-- fillEmptyWithMean :: [[Double]] -> [[Double]]
-- fillEmptyWithMean rows =
--     let
--         colsCnt = getN $ head rows
--         means = map (getColMean cols) [0..colsCnt]
--     in
--         replaceEmptyWithMean rows means
--
--
-- getColsData :: [[String]] -> [(Double)]


selectCols :: [Int] -> [String] -> [String]
selectCols usedCols cols =
    map (\idx -> cols!!idx) usedCols


parseRow :: [Int] -> [String] -> [Double]
parseRow usedCols row =
    map read $ selectCols usedCols row


parseRows :: [Int] -> [[String]] -> [[Double]]
parseRows usedCols rows =
    map (parseRow usedCols) rows


readData :: FilePath -> [Int] -> IO [[Double]]
readData filePath usedCols = do
    content <- readFile filePath
    return $ parseRows usedCols $ map splitLine $ lines content


-- Clustering --

diff :: [Double] -> [Double] -> Double
diff a b =
    foldl (+) 0 $ map (\(ai, bi) -> abs (ai - bi)) $ zip a b


assignCluster :: [(Cls, [Double])] -> [Double]-> (Cls, [Double])
assignCluster centers cols =
    let
        diffs = map (\(cls, center) -> (cls, diff center cols)) centers
        bestMatch = head $ sortOn snd diffs
    in
        (fst bestMatch, cols)


remapToClusters :: [(Cls, [Double])] -> [[(Cls, [Double])]] -> [(Cls, [[Double]])]
remapToClusters centers d =
    let
        newClusters = map (\cluster -> (fst $ head cluster, map snd cluster)) d
    in
        map (\cls -> (cls, maybe [] snd $ find (\x -> cls == fst x) newClusters)) $ map fst centers


cluster :: [(Cls, [Double])] -> [[Double]]-> [(Cls, [[Double]])]
cluster centers ds =
    remapToClusters centers $ groupBy (\a b -> (fst a) == (fst b)) $ sortOn fst $ map (assignCluster centers) ds


mean :: [Double] -> Double
mean ds = sum ds / fromIntegral (length ds)


getCenter :: [[Double]] -> [Double]
getCenter ds = map (\idx -> mean $ map (!!idx) ds) $ [0..getN $ head ds]


getCenters :: [(Cls, [[Double]])] -> [(Cls, [Double])]
getCenters = map (\(cls, ds) -> (cls, getCenter ds))


-- Error evaluation --

getMaxDiff :: [(Cls, [Double])] -> [(Cls, [Double])] -> Double
getMaxDiff newCenters oldCenters =
    maximum $ map (\(a, b) -> diff a b) $ zipWith (\a b -> (snd a, snd b)) newCenters oldCenters


isCloseEnough :: [(Cls, [Double])] -> [(Cls, [Double])] -> Bool
isCloseEnough newCenters oldCenters = (getMaxDiff newCenters oldCenters) < 0.000001


isEnough :: Int -> [(Cls, [Double])] -> [(Cls, [Double])] -> Bool
isEnough iteration newCenters oldCenters =
    iteration > 20 || isCloseEnough newCenters oldCenters


-- K Means --

kMeans :: Int -> [(Cls, [Double])] -> [[Double]] -> (Int, [(Cls, [Double])], [(Cls, [[Double]])])
kMeans iteration centers ds =
    let
        clustered = cluster centers ds
        newCenters = getCenters clustered
    in
        if isEnough iteration newCenters centers
            then
                (iteration, newCenters, clustered)
            else
                kMeans (iteration + 1) newCenters ds


-- Printing --

printClusterSummary :: Int -> (Cls, [[Double]]) -> IO ()
printClusterSummary total (cls, cluster) =
    let
        l = length cluster
        percentage = roundTo 2 (100.0 * (fromIntegral l) / (fromIntegral total))
    in
        putStrLn $ "Cluster " ++ (show cls) ++ ": " ++ (show $ length cluster) ++ " (" ++ show percentage ++ " %)"


printClusterCenter :: (Cls, [Double]) -> IO ()
printClusterCenter (cls, centers) =
    putStrLn $ "Cluster " ++ (show cls) ++ ": " ++ (foldl (\acc c -> acc ++ (show $ roundTo 4 c) ++ " ") "" centers)


-- Main --

main :: IO ()
main = do
    (fileName:clustersCntStr:usedColsStrs) <- getArgs
    putStrLn ""
    putStrLn "=== K-Means ==="

    let clustersCnt = read clustersCntStr :: Int
    let usedCols = map read usedColsStrs :: [Int]

    putStrLn $ "Clusters count: " ++ (show clustersCnt)
    putStrLn $ "Used columns: [" ++ (foldl (\acc c -> acc ++ (show c) ++ " ") " " usedCols) ++ "]"

    putStrLn ""
    putStrLn "-- Reading data --"
    putStrLn $ "File: " ++ fileName

    ds <- readData fileName usedCols
    let dataSetSize = length ds

    putStrLn $ "Dataset size: " ++ (show dataSetSize)
    putStrLn $ "Columns count: " ++ (show $ length $ head ds)

    putStrLn ""
    putStrLn "-- Running K-Means --"

    g <- getStdGen
    let initialCentersIdx = take clustersCnt (randomRs (0, dataSetSize - 1) g) :: [Int]
    let initialCenters = zip [0..clustersCnt-1] $ map (\idx -> ds!!idx) initialCentersIdx

    putStrLn "Starting points:"
    mapM printClusterCenter initialCenters

    let (iterations, centers, clusters) = kMeans 1 initialCenters ds
    putStrLn ""
    putStrLn $ "Iterations count: " ++ (show iterations)

    putStrLn ""
    putStrLn "-- Results --"
    putStrLn "Final cluster centroids:"
    mapM printClusterCenter centers

    putStrLn ""
    putStrLn "Clustered instances:"
    mapM (printClusterSummary dataSetSize) clusters

    putStrLn ""
    putStrLn "Done."
