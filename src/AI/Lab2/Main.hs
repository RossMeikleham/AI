
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


-- Given a filename and a list of values, writes
-- the given values to the file. One per each line.
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
    let speechSampleFiles = filter (isInfixOf "speech") sampleFiles
    let silenceSampleFiles = filter (isInfixOf "silence") sampleFiles


    -- Extract samples from all the files
    speechSamples <- mapM getSamples speechSampleFiles
    silenceSamples <- mapM getSamples silenceSampleFiles

    let samples = speechSamples ++ silenceSamples

    -- Log of Average Short Term Energy
    let logAvgEnergy = logAverageSig samples win_sz_ms energy
    writeDat "log_avg_ste" logAvgEnergy
     
    -- Log of Average Short Term Magnitude Signal
    let logAvgMtude = logAverageSig samples win_sz_ms magnitude
    writeDat "log_avg_stm" logAvgMtude

    -- Average Zero Crossing Rate Signal
    let avgZCR = averageSig samples win_sz_ms zeroCrossingRate
    writeDat "avg_zcr" avgZCR
    
    let speechVars = [logAverageSig speechSamples win_sz_ms energy,
                      logAverageSig speechSamples win_sz_ms magnitude,
                      averageSig speechSamples win_sz_ms zeroCrossingRate]

    let speechData = map (\d -> (d, Speech)) $ 
            transpose speechVars
    
    let silenceVars = [logAverageSig silenceSamples win_sz_ms energy,
                       logAverageSig silenceSamples win_sz_ms magnitude,
                       averageSig silenceSamples win_sz_ms zeroCrossingRate]

    let silenceData = map (\d -> (d, Silence)) $ 
            transpose silenceVars

    -- Randomly split speech and silence data into 
    -- 10 equally sized disjoint subsets
    speechSplits <- splitK speechData 10
    silenceSplits <- splitK silenceData 10

    -- Concatonate each silence and speech splits, obtain the training/test
    -- sets and calculate the average loss using a 10-fold cross
    -- validation.
    let splits = zipWith (++) speechSplits silenceSplits

    let trainTest = getTrainTest splits
    
    let avgLoss = cvAverageLoss trainTest
    
    print avgLoss 
    where 
        win_sz_ms = 15



