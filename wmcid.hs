import Data.Maybe(fromMaybe)
import Data.List(isPrefixOf)
import Control.Concurrent(threadDelay)
import System.IO(hGetContents)
import System.Process(proc, createProcess, StdStream(CreatePipe), std_out, std_err, waitForProcess)

-- YOLO, let's rewrite bash pipe |o/
a ! b = b a

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

mainCpuUsage :: IO Float
mainCpuUsage = do
    firstTotalIdle <- getTotalIdle
    threadDelay 1000000 -- wait one second
    secondTotalIdle <- getTotalIdle
    numberOfCpu <- getNumberOfCpu
    return $ 100.0 - ((secondTotalIdle - firstTotalIdle) / fromIntegral numberOfCpu)
  where parseCpuInfos :: String -> [Float]
        parseCpuInfos procStatContent = ((filter (\line -> (line !! 0 == "cpu")) $ procStatContent ! lines ! (map words)) !! 0) ! drop 1 ! map (\x -> x ! read :: Float)
        getTotalIdle :: IO Float
        getTotalIdle = readFile "/proc/stat" >>= \x -> return $! (parseCpuInfos x) !! 3

main = putStrLn "pouet"
