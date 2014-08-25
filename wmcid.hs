import Data.Text(pack, split, unpack)
import qualified Data.Text(length)
import Data.Maybe(fromMaybe)
import Data.List(isPrefixOf)
import Control.Concurrent(threadDelay)
import System.IO(hGetContents)
import System.Process(proc, createProcess, StdStream(CreatePipe), std_out, std_err, waitForProcess)

-- YOLO, let's rewrite bash pipe |o/
a ! b = b a

toLineOfWords :: String -> [[String]]
toLineOfWords = (map words) . lines

data Service = Service {
        name :: String,
        running :: Bool
    } deriving Show


-- taken from missingH which fails to install with cabal
startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

splitL :: Eq a => [a] -> [a] -> [[a]]
splitL _ [] = []
splitL delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else splitL delim
                                                 (drop (length delim) x)

wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x wschars
                            then lstrip xs
                            else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
-- end of missingH

availableServices :: IO [Service]
availableServices = do
    (_, Just hout, Just herr, pid) <- createProcess (proc "service" ["--status-all"]){ std_out = CreatePipe, std_err = CreatePipe }
    waitForProcess pid
    services <- hGetContents hout
    return $ map (\line -> Service (drop 8 line) (if (line !! 3) == '+' then True else False)) (lines services)

getNumberOfCpu :: IO Int
getNumberOfCpu = readFile "/proc/cpuinfo" >>= return . length . filter (isPrefixOf "processor") . lines

parseCpuInfos :: String -> [Int]
parseCpuInfos procStatContent = getCpuLineAsWords ! drop 1 ! map (\x -> x ! read :: Int)
    where getCpuLineAsWords = ((filter (\line -> (line !! 0 == "cpu")) $ toLineOfWords procStatContent) !! 0)

parseAllCpuInfos :: String -> [Int]
parseAllCpuInfos procStatContent = (filter (\line -> (isPrefixOf "cpu" (line !! 0) && (line !! 0) /= "cpu")) $ toLineOfWords procStatContent) ! map (\x -> (x !! 4 ! read :: Int))

getLoadAvg :: IO [Float]
getLoadAvg = do
    readFile "/proc/loadavg" >>= return . map (\x -> read x :: Float) . take 3 . words

getMemInfo :: IO [(String, Int)]
getMemInfo = do
    readFile "/proc/meminfo" >>= return . parseMemInfo . lines
    where parseMemInfo = map parseMemInfoLine
          parseMemInfoLine line = line ! words ! init ! map pack ! concatMap (split (== ':')) ! (\c -> (c !! 0 ! unpack, c !! 2 ! unpack ! read :: Int))

getCpuInfo :: IO [(Int, [(String, String)])]
getCpuInfo = do
    readFile "/proc/cpuinfo" >>= return . map extractCpuIdFromBlock . map transformBlockToDict . splitIntoCpuInfoBlocks
    where splitIntoCpuInfoBlocks :: String -> [String]
          splitIntoCpuInfoBlocks = filter ((/= 0) . length) . splitL ("\n\n")
          transformBlockToDict :: String -> [(String, String)]
          transformBlockToDict = map ((\[a, b] -> (a, b)) . (map strip . splitL ":")) . lines
          extractCpuIdFromBlock :: [(String, String)] -> (Int, [(String, String)])
          extractCpuIdFromBlock block = (getCpuId block, block)
          getCpuId :: [(String, String)] -> Int
          getCpuId block = ((filter (\(a, b) -> a == "processor") block) !! 0) ! snd ! read :: Int

mainCpuUsage :: IO Float
mainCpuUsage = do
    firstTotalIdle <- getTotalIdle
    threadDelay 1000000 -- wait one second
    secondTotalIdle <- getTotalIdle
    numberOfCpu <- getNumberOfCpu
    return $ 100.0 - ((fromIntegral (secondTotalIdle - firstTotalIdle)) / (fromIntegral numberOfCpu))
  where getTotalIdle :: IO Int
        getTotalIdle = readFile "/proc/stat" >>= \x -> return $! (parseCpuInfos x) !! 3

allCpuUsage :: IO [(Int, Int)]
allCpuUsage = do
    firstAllCpuTotalIdle <- getAllCpuTotalIdle
    threadDelay 1000000 -- wait one second
    secondAllCpuTotalIdle <- getAllCpuTotalIdle
    return $ zip [1..] $ zip secondAllCpuTotalIdle firstAllCpuTotalIdle ! map (\(a, b) -> 100 + b - a)
  where getAllCpuTotalIdle = readFile "/proc/stat" >>= \x -> return $! (parseAllCpuInfos x)

main = putStrLn "pouet"
