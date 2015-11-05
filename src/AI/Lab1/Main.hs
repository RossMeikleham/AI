import System.IO
import qualified Data.Vector.Unboxed as VU

import AI.Lab1.Lab1

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


