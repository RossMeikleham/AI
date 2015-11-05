
import System.IO
import System.Directory
import Data.List 
import System.FilePath
import qualified Data.Vector.Unboxed as VU

import AI.Lab2.Lab2
import AI.Lab1.Lab1

samplesDir :: String
samplesDir = "samples"

-- Obtain samples from file
getSamples :: String -> IO [Int]
getSamples fPath = do
    handle <- openFile fPath ReadMode  
    contents <- hGetContents handle

    -- Obtain a list of samples (S[n]) as strings from file
    let strSamples = lines contents 

    -- Convert samples to integers
    let samples = map (\s -> (read s :: Int)) strSamples 

    return samples

writeDat :: String -> [Double] -> IO ()
writeDat fName signals = writeFile (fName ++ ".dat") $
                            unlines $ map show signals
    


main :: IO ()
main = do
    -- Obtain absolute paths to all files in the given folder
    folderFiles <- getDirectoryContents samplesDir >>= 
                        mapM (canonicalizePath . (samplesDir </>))

    -- Filter to obtain paths for only the sample files
    let sampleFiles = filter (isSuffixOf ".dat") folderFiles

    -- Extract samples from all the files
    samples <- mapM getSamples sampleFiles
    print $ length samples

    -- Log of Average Short Term Energy
    let log_avg_e = logAverageSig samples win_sz_ms energy
    writeDat "log_avg_ste" log_avg_e
     
    -- Log of Average Short Term Magnitude Signal
    let log_avg_mtude = logAverageSig samples win_sz_ms magnitude
    writeDat "log_avg_stm" log_avg_mtude

    -- Average Zero Crossing Rate Signal
    let avg_zcr = averageSig samples win_sz_ms zeroCrossingRate
    writeDat "avg_zcr" avg_zcr

    putStrLn "done"
    where 
        win_sz_ms = 15


