
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


-- Apply moving average with k1, k2 to a given list of signals
--movingAverage :: [Int] -> Int -> Int -> Maybe [Int]
--movingAverage signals k1 k2 = 


-- Plot samples
plotSamples :: [Int] -> [(String, [Int])] -> IO ()
plotSamples xs ys = do
    plot X11 $ [Data2D [Title "Sample Data", Style Lines] [] $ points $ normalise xs]
              ++ others
    return ()

    --lotList [] (points (normalise xs))
    where
        others = map (\(g,col) -> Data2D [Title (fst g), Style Lines, Color col] [] 
                            $ points $ normalise $ snd g) (zip ys (map getColor [0..]))

        -- Normalise a set of samples between 1.0 and -1.0
        normalise :: [Int] -> [Double] 
        normalise = map (\y -> (fromIntegral y) / (fromIntegral $ maximum xs))

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
    plotSamples samples $ zip ["5ms delay", "10ms delay", "15ms delay"] (catMaybes iods)
     
    hClose handle      


