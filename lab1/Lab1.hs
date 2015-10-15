import Data.Maybe
import System.IO
import qualified Data.Vector.Unboxed as VU

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
idealOperatorDelay :: [Int] -> Int -> [Int]
idealOperatorDelay samples 0 = samples --Delaying by 0 ms gives back the same sample
idealOperatorDelay samples ms =  
        -- Remove last n ms from samples
        -- Pad the front n ms with samples containing 0 
        take n (repeat 0) ++ 
            take (length samples - n) samples 
    where 
          samplesPerMs = (length samples `div` samplingTime)
          n = samplesPerMs * ms 


-- Apply moving average with k1, k2 ms to a given list of signals
movingAverage :: [Int] -> Int -> Int -> [Double]
movingAverage samples 0 0 = map fromIntegral samples
movingAverage samples k1 k2 = 
    map y [0,1..(length samples - 1)]
    where 
          samplesPerMs = (length samples `div` samplingTime)
          nk1 = samplesPerMs * k1
          nk2 = samplesPerMs * k2

          y :: Int -> Double
          y n = let a = max 1 (n - nk1) 
                    b = min (length samples - 1) (n + nk2)
                in  fromIntegral (sum $ (take (b - a + 1)) $ drop (a - 1) samples) /
                    fromIntegral (b - a + 1)
            

-- Apply convolution to a given vector of samples with a window
-- of given length in miliseconds
convolute :: VU.Vector Int -> Int -> VU.Vector Int
convolute samples win_sz = VU.map y $ VU.fromList [0..(numSamples - 1)]
    
  where y :: Int -> Int
        y n = VU.foldl' (+) 0 $ -- Sum the results 
                VU.map (\k -> (samples VU.! k) * window (n - k)) $ -- s[k] * w[n - k]
                     VU.fromList [0,1..(min n (numSamples - 1))] -- k values
          
        win_sz_samples = (numSamples `div` samplingTime) * win_sz

        -- Rectangular Window function, 1 inside the window, 0 outside
        window :: Int -> Int
        window n 
            | (n < 0) = 0 -- Left of window
            | (n >= win_sz_samples) = 0 -- Right of window
            | otherwise = 1 -- Inside the Window
        
        numSamples = VU.length samples    


plotToCsv :: Real a => String -> [a] -> IO ()
plotToCsv name graph = 
    writeFile (name ++ ".csv") $ 
        unlines $ map (\(x, y) -> concat [show x, ",", show y]) $ 
                      points $ map realToDouble graph
  where
    
    -- Generate (x,y) points from a list of samples
    points :: [Double] -> [(Double, Double)] 
    points samples = zip
                -- x coodinates  
                (map (\x -> fromIntegral x * 
                                (fromIntegral samplingTime / 
                                (fromIntegral $ length samples))
                     )  [0..]) 
                 -- y coordinates
                 samples

    realToDouble :: (Real a) => a -> Double
    realToDouble = fromRational . toRational


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
    
    -- Calc ideal operator delays for 5, 10, and 15 ms, and plot against
    -- samples
    let iods = map (idealOperatorDelay samples) [5, 10, 15]
--    plotSamples samples $ zip ["5ms delay", "10ms delay", "15ms delay"] iods
    
    -- Calc moving averages for k1=k2=5, 10, and 15ms, and plot against samples
    let mas = map (\(k1,k2) -> movingAverage samples k1 k2) [(5, 5), (10, 10), (15, 15)]
--    plotSamples samples $ zip ["MA 5ms", "MA 10ms", "MA 15ms"] mas
   
    -- Calc convolution for window size of 10ms
    let cvs = convolute (VU.fromList samples) 10
    plotToCsv "Convolution" (VU.toList cvs) 

    hClose handle      


