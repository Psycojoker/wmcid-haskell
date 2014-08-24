import Data.Maybe(fromMaybe)
import System.IO(hGetContents)
import System.Process(proc, createProcess, StdStream(CreatePipe), std_out, std_err, waitForProcess)

availableServices :: IO [(Bool, String)]
availableServices = do
    (_, Just hout, Just herr, pid) <- createProcess (proc "service" ["--status-all"]){ std_out = CreatePipe, std_err = CreatePipe }
    waitForProcess pid
    services <- hGetContents hout
    return $ map (\line -> (if (line !! 3) == '+' then True else False, drop 8 line)) (lines services)

main = putStrLn "pouet"
