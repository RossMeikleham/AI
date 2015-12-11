module AI.Lab1.Lab1 where

import Data.Maybe
import System.IO
import qualified Data.Vector.Unboxed as VU

-- | Sampling time for all samples in milliseconds
samplingTime :: Int
samplingTime = 300


-- | Calculate the sampling rate from a signal
samplingRate :: [Int] -> Double
samplingRate samples = fromIntegral (length samples) / 
                       (fromIntegral (samplingTime) / 1000.0)


-- Apply ideal operator delay to a signal (S[n]) for 
-- a delay of m ms
idealOperatorDelay :: [Int] -> Int -> [Int]

--Delaying by 0 ms gives back the same signal
idealOperatorDelay signal 0 = signal

idealOperatorDelay signal ms =  
        -- Remove last n ms from signal
        -- Pad the front n ms with 0 
        take n (repeat 0) ++ 
            take (length signal - n) signal
    where 
          samplesPerMs = (length signal `div` samplingTime)
          n = samplesPerMs * ms 


-- Apply moving average with k1, k2 ms 
-- to a given signal
movingAverage :: [Int] -> Int -> Int -> [Double]
movingAverage signal 0 0 = map fromIntegral signal
movingAverage signal k1 k2 = 
    map y [0,1..(length signal - 1)]
    where 
          samplesPerMs = (length signal `div` samplingTime)
          nk1 = samplesPerMs * k1
          nk2 = samplesPerMs * k2

          y :: Int -> Double
          y n = let a = max 1 (n - nk1) 
                    b = min (length signal - 1) (n + nk2)
                in  fromIntegral (sum $ (take (b - a + 1)) $ 
                                         drop (a - 1) signal) /
                    fromIntegral (b - a + 1)
            

-- Rectangular Window function which takes the size of the 
-- window "win_sz" and "n", returns 1 if n is inside the window, 
-- 0 otherwise 
rectWindow :: Int -> Int -> Int
rectWindow win_sz n
    | n < 0 = 0 -- Left of Window
    | n >= win_sz = 0 -- Right of Window
    | otherwise = 1 -- Inside the Window


-- Apply convolution to a signal with a window
-- of given length in milliseconds
convolute :: VU.Vector Int -> Int -> VU.Vector Int
convolute signal win_sz = 
            VU.map y $ VU.fromList [0..(numSamples - 1)]
    
  where y :: Int -> Int
        y n = VU.foldl' (+) 0 $ -- Sum the results
                -- s[k] * w[n - k] 
                VU.map (\k -> (signal VU.! k) * 
                              (rectWindow win_sz_samples (n - k))) $ 
                     -- k values
                     VU.fromList [(max (n - win_sz_samples + 1) 0) .. 
                                  (min n (numSamples - 1))] -- k values
          
        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length signal    


-- Calculate energy for a given signal with a window
-- of given length in milliseconds
energy :: VU.Vector Int -> Int -> VU.Vector Double
energy signal win_sz = VU.map e $ VU.fromList [0..(numSamples - 1)]
    
  where e :: Int -> Double
        -- sum(s[k]^2 * w[n-k])/N
        e n = (fromIntegral $ sumRes n) / (fromIntegral win_sz_samples) 
        
        sumRes :: Int -> Int
        sumRes n = 
            VU.foldl'(+) 0 $ -- Sum results
                -- s[k]^2 * w[n-k]
                VU.map (\k -> ((signal VU.! k) ^ 2) * 
                       (rectWindow win_sz_samples (n - k))) $ 
                        -- k values
                        VU.fromList [(max (n - win_sz_samples + 1) 0) .. 
                                              (min n (numSamples - 1))] 

        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length signal    


-- Calculate magnitude for a given signal with a window
-- of given length in milliseconds
magnitude :: VU.Vector Int -> Int -> VU.Vector Double
magnitude signal win_sz = VU.map m $ VU.fromList [0..(numSamples - 1)]
    
  where m :: Int -> Double
        -- sum(|s[k]| * w[n-k])/N
        m n = (fromIntegral $ sumRes n) / (fromIntegral win_sz_samples) 
        
        sumRes n = VU.foldl'(+) 0 $ -- Sum results
                      -- |s[k]| * w[n-k]
                      VU.map (\k -> ((abs (signal VU.! k))) * 
                             (rectWindow win_sz_samples (n - k))) $ 
                          -- k values
                          VU.fromList [(max (n - win_sz_samples + 1) 0) .. 
                                         (min n (numSamples - 1))] 

        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length signal  


-- Calculate zero crossing rate for a given signal with a window
-- of given length in milliseconds
zeroCrossingRate :: VU.Vector Int -> Int -> VU.Vector Double
zeroCrossingRate signal win_sz = 
    VU.map m $ VU.fromList [0..(numSamples - 1)]
    
  where m :: Int -> Double
        -- sum(|s[k]| * w[n-k])/N
        m n = (fromIntegral $ sumRes n) / 
                    (2 * (fromIntegral win_sz_samples)) 
        
        sumRes n = VU.foldl'(+) 0 $ -- Sum results
                      -- |sgn(s[k] - sgn(s[k-1])| * w[n-k]
                     VU.map (\k -> (abs (signum (signal VU.! k) - 
                                        (signum (signal VU.! (k - 1))))) * 
                                        (rectWindow win_sz_samples (n - k)))
                            -- k values
                            $ VU.fromList 
                                    [(max (n - win_sz_samples + 1) 1) .. 
                                     (min n (numSamples - 1))] 

        -- Number of samples in the Window of win_sz milliseconds 
        win_sz_samples = (numSamples `div` samplingTime) * win_sz
        -- Total number of samples supplied to the convolution function
        numSamples = VU.length signal  


-- Write Samples to CSV file
plotToCsv :: Real a => String -> [a] -> IO ()
plotToCsv name graph = 
    writeFile (name ++ ".csv") $ 
        unlines $ map (\(x, y) -> concat [show x, ",", show y]) $ 
                      points $ map realToDouble graph
  where
    
    -- Generate (x,y) points from a signal
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
