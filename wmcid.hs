import Data.Maybe(fromMaybe)
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

mainCpuUsage :: IO Float
mainCpuUsage = do
    [_, _, _, firstTotalIdle, _, _, _, _, _, _] <- readFile "/proc/stat" >>= return . getCpuInfos
    threadDelay 1000000 -- wait one second
    [_, _, _, secondTotalIdle, _, _, _, _, _, _] <- readFile "/proc/stat" >>= return . getCpuInfos
    -- return $ (secondTotalIdle ! read :: Int) - (firstTotalIdle :: read :: Int)
    return $ 100.0 - ((secondTotalIdle - firstTotalIdle) / 8.0)
  where getCpuInfos :: String -> [Float]
        getCpuInfos procStatContent = ((filter (\line -> (line !! 0 == "cpu")) $ procStatContent ! lines ! (map words)) !! 0) ! drop 1 ! map (\x -> x ! read :: Float)

main = putStrLn "pouet"
