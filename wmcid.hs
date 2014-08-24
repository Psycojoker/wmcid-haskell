import Data.Maybe(fromMaybe)
import System.IO(hGetContents)
import System.Process(proc, createProcess, StdStream(CreatePipe), std_out, std_err, waitForProcess)

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

main = putStrLn "pouet"
