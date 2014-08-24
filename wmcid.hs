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
