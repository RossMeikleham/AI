import Data.Maybe
import System.IO
import Graphics.EasyPlot 

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
            

-- Apply convolution with a given list of samples with a window
-- of given length in miliseconds
convolute :: [Int] -> Int -> [Int]
convolute samples win_sz = window 800 --map ((x, y) -> x * y) (zip samples window)
    
    where window :: Int -> [Int]
          window n = let
            left = take (max 0 (n - win_sz_samples)) $ repeat 0
            win  = take win_sz_samples $ repeat $ maximum samples
            right = take (length samples - length left - length win) $ repeat 0
            in left ++ win ++ right

          win_sz_samples = (length samples `div` samplingTime) * win_sz
                     
          

-- Plot samples
plotSamples :: (Real a, Real b) => [a] -> [(String, [b])] -> IO ()
plotSamples xs ys = do
    plot X11 $ [Data2D [Title "Sample Data", Style Lines] [] $ points $ normalise xs]
              ++ others
    return ()

    --lotList [] (points (normalise xs))
    where
        others = map (\(g,col) -> Data2D [Title (fst g), Style Lines, Color col] [] 
                            $ points $ normalise $ snd g) (zip ys (map getColor [0..]))

        -- Normalise a set of samples between 1.0 and -1.0
        normalise :: (Real a) => [a] -> [Double] 
        normalise = map (\y -> (2 * ((realToDouble y) - (realToDouble $ minimum xs))) / 
                               (realToDouble (maximum xs - minimum xs)) - 1.0)

                        where realToDouble :: (Real a) => a -> Double
                              realToDouble = fromRational . toRational

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

        -- Select line color from a selection of 7 colors
        getColor :: Int -> Color
        getColor n = case n `mod` 7 of
            0 -> Red
            1 -> Green
            2 -> Black
            3 -> Orange
            4 -> Yellow
            5 -> DarkOrange
            _ -> Cyan

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
    plotSamples samples $ zip ["5ms delay", "10ms delay", "15ms delay"] iods
    
    -- Calc moving averages for k1=k2=5, 10, and 15ms, and plot against samples
    let mas = map (\(k1,k2) -> movingAverage samples k1 k2) [(5, 5), (10, 10), (15, 15)]
    plotSamples samples $ zip ["MA 5ms", "MA 10ms", "MA 15ms"] mas
   
    -- Calc convolution for window size of 10ms
    let cvs = convolute samples 10
    plotSamples samples [("Window", cvs)]

    hClose handle      


