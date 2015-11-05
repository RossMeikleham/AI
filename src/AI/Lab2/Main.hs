
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

    -- Average Short Term Energy 
    let st_energies = map (\m -> energy (VU.fromList m) win_sz_ms) samples :: [VU.Vector Double]
    let sum_st_energies = map (VU.foldl' (+) 0)  st_energies
    let avg_st_energies = map (/ (fromIntegral $ length st_energies)) sum_st_energies
    putStrLn "done"

    where 
        win_sz_ms = 15


