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
            

-- Rectangular Window function which takes the size of the window "win_sz"
-- and "n", returns 1 if n is inside the window, 0 otherwise 
rectWindow :: Int -> Int -> Int
rectWindow win_sz n
    | n < 0 = 0 -- Left of Window
    | n >= win_sz = 0 -- Right of Window
    | otherwise = 1 -- Inside the Window


-- Apply convolution to a given vector of samples with a window
-- of given length in milliseconds
convolute :: VU.Vector Int -> Int -> VU.Vector Int
convolute samples win_sz = VU.map y $ VU.fromList [0..(numSamples - 1)]
    
  where y :: Int -> Int
        y n = VU.foldl' (+) 0 $ -- Sum the results 
                VU.map (\k -> (samples VU.! k) * (rectWindow win_sz_samples (n - k))) $ -- s[k] * w[n - k]
                     VU.fromList [(max (n - win_sz_samples + 1) 0) .. (min n (numSamples - 1))] -- k values
          
        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length samples    


-- Calculate energy for a given vector of samples with a window
-- of given length in milliseconds
energy :: VU.Vector Int -> Int -> VU.Vector Double
energy samples win_sz = VU.map e $ VU.fromList [0..(numSamples - 1)]
    
  where e :: Int -> Double
        e n = (fromIntegral $ sumRes n) / (fromIntegral win_sz_samples) -- sum(s[k]^2 * w[n-k])/N
        
        sumRes :: Int -> Int
        sumRes n = VU.foldl'(+) 0 $ -- Sum results
                        VU.map (\k -> ((samples VU.! k) ^ 2) * (rectWindow win_sz_samples (n - k))) $ -- s[k]^2 * w[n-k]
                            VU.fromList [(max (n - win_sz_samples + 1) 0) .. (min n (numSamples - 1))] -- k Values

        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length samples    


-- Calculate magnitude for a given vector of samples with a window
-- of given length in milliseconds
magnitude :: VU.Vector Int -> Int -> VU.Vector Double
magnitude samples win_sz = VU.map m $ VU.fromList [0..(numSamples - 1)]
    
  where m :: Int -> Double
        m n = (fromIntegral $ sumRes n) / (fromIntegral win_sz_samples) -- sum(|s[k]| * w[n-k])/N
        
        sumRes n = VU.foldl'(+) 0 $ -- Sum results
                        VU.map (\k -> ((abs (samples VU.! k))) * (rectWindow win_sz_samples (n - k))) $ -- |s[k]| * w[n-k]
                            VU.fromList [(max (n - win_sz_samples + 1) 0) .. (min n (numSamples - 1))] -- k Values

        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length samples    


-- Calculate zero crossing rate for a given vector of samples with a window
-- of given length in milliseconds
zeroCrossingRate :: VU.Vector Int -> Int -> VU.Vector Double
zeroCrossingRate samples win_sz = VU.map m $ VU.fromList [0..(numSamples - 1)]
    
  where m :: Int -> Double
        m n = (fromIntegral $ sumRes n) / (2 * (fromIntegral win_sz_samples)) -- sum(|s[k]| * w[n-k])/N
        
        sumRes n = VU.foldl'(+) 0 $ -- Sum results
                        VU.map (\k -> (abs (signum (samples VU.! k) - (signum (samples VU.! (k - 1))))) * 
                                      (rectWindow win_sz_samples (n - k))) $ -- |sgn(s[k]) - sgn(s[k-1])| * w[n-k]
                            VU.fromList [(max (n - win_sz_samples + 1) 1) .. (min n (numSamples - 1))] -- k Values

        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length samples    


-- Write Samples to CSV file
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
    
    -- Generate CSV file of original data
    plotToCsv "Laboratory" samples

    -- Calc ideal operator delays for 5, 10, and 15 ms, and generate CSV
    -- files
    let iods = map (idealOperatorDelay samples) [5, 10, 15]
    mapM_ (uncurry plotToCsv) $ zip ["Delay5", "Delay10", "Delay15"] iods
    
    -- Calc moving averages for k1=k2=5, 10, and 15ms, and generate 
    -- CSV files
    let mas = map (\(k1,k2) -> movingAverage samples k1 k2) [(5, 5), (10, 10), (15, 15)]
    mapM_ (uncurry plotToCsv) $ zip ["MAverage5", "MAverage10", "MAverage15"] mas
   
    -- Calc convolution for window size of 10ms, and generate CSV file
    let cvs = convolute (VU.fromList samples) 10
    plotToCsv "Convolution" (VU.toList cvs) 

    -- Calc short term energy signal for 30ms, and generate CSV file
    let ste = energy (VU.fromList samples) 30
    plotToCsv "Energy" (VU.toList ste)

    -- Calc short term magnitude for 30ms, and generate CSV file
    let md = magnitude (VU.fromList samples) 30
    plotToCsv "Magnitude" (VU.toList md)

    -- Calc short term ZCR for 30s, and generate CSV file
    let zcr = zeroCrossingRate (VU.fromList samples) 30
    plotToCsv "ZeroCrossingRate" (VU.toList zcr)
    
    hClose handle      


