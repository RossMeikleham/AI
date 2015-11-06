
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
    let speechSampleFiles = filter (isPrefixOf "speech") sampleFiles
    let silenceSampleFiles = filter (isPrefixOf "silence") sampleFiles

    -- Extract samples from all the files
    speechSamples <- mapM getSamples speechSampleFiles
    silenceSamples <- mapM getSamples silenceSampleFiles

    let samples = speechSamples ++ silenceSamples
    print $ length samples

    -- Log of Average Short Term Energy
    let logAvgEngery = logAverageSig samples win_sz_ms energy
    writeDat "log_avg_ste" logAvgEnergy
     
    -- Log of Average Short Term Magnitude Signal
    let logAvgMtude = logAverageSig samples win_sz_ms magnitude
    writeDat "log_avg_stm" logAvgMtude

    -- Average Zero Crossing Rate Signal
    let avgZCR = averageSig samples win_sz_ms zeroCrossingRate
    writeDat "avg_zcr" avgZCR

    speechSplits <- splitK 5 speechSamples
    silenceSplits <- splitK 5 silenceSamples

    putStrLn "done"
    where 
        win_sz_ms = 15


