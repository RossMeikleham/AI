import System.IO
import Graphics.EasyPlot as P

-- | Sampling time for all samples in milliseconds
samplingTime :: Int
samplingTime = 300

-- | Calculate the sampling rate from a given
--   list of samples
samplingRate :: [Int] -> Double
samplingRate samples = fromIntegral (length samples) / 
                       (fromIntegral (samplingTime) / 1000.0)

-- Apply ideal operator delay to a list of samples (S[n]) for 
-- a delay of m ms
idealOperatorDelay :: [Int] -> Int -> Maybe [Int]
idealOperatorDelay samples 0 = Just samples --Delaying by 0 ms gives back the same sample
idealOperatorDelay samples ms =  
    if (samplingTime `mod` ms == 0) && (length samples `mod` offsetRate == 0)  
        -- Remove last n ms from samples
        -- Pad the front n ms with samples containing 0 
        then Just $ take offset (repeat 0) ++ 
                    take (length samples - offset) samples 

        else Nothing
    where 
          offsetRate = (samplingTime `div` ms)
          offset = (length samples) `div` offsetRate  --Number of samples to "delay"


normaliseMultiplier :: [Int] -> Double
normaliseMultiplier xs = 1.0 / (fromIntegral $ maximum xs)

-- Plot samples
plotSamples :: [Int] -> [[Int]] -> IO ()
plotSamples xs ys = do
    plot X11 $ Data2D [Title "Sample Data", Style Lines] [] $ points $ normalise xs
    return ()

    --lotList [] (points (normalise xs))
    where
        normalise :: [Int] -> [Double] 
        normalise = map (\y -> (fromIntegral y) / (fromIntegral $ maximum xs))

        points :: [Double] -> [(Double, Double)] 
        points samples = zip  
                    (map (\x -> fromIntegral x * 
                                    (fromIntegral samplingTime / 
                                    (fromIntegral $ length samples))
                         )  [0..]) 
                     samples -- y co-ordinates

labFilePath :: String
labFilePath = "laboratory.dat"

main :: IO ()
main = do
    handle <- openFile labFilePath ReadMode  
    contents <- hGetContents handle
    -- Obtain a list of samples (S[n]) as strings from file
    let strSamples = lines contents 
    -- Convert samples to integers
    let samples = map (\s -> (read s :: Int)) strSamples 
    -- Calculate sampling rate 
    putStr $ "Sampling rate is: " ++ (show  $ samplingRate samples) ++ "Hz\n"
    -- Plot our samples
    plotSamples samples [] 
    hClose handle      
