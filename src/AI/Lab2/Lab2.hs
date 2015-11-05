module AI.Lab2.Lab2 where

import qualified Data.Vector.Unboxed as VU


-- Given a list of list of signals, a window length and a function which
-- takes a list of signals and that window lenth and transforms the signal;
-- this function takes the average of the transformation on the signals
averageSig :: [[Int]] -> Int -> (VU.Vector Int -> Int -> VU.Vector Double) -> [Double]
averageSig signals ms f = avg_f_signals
    where f_signals = map (\m -> f (VU.fromList m) ms ) signals -- Apply transformation
          sum_f_signals = map (VU.foldl' (+) 0) f_signals -- Sum signals
          avg_f_signals = map (/ (fromIntegral $ length signals)) sum_f_signals -- Average signals

-- Given a list of list of signals, a window length and a function which
-- takes a list of signals and that window lenth and transforms the signal;
-- this function takes the log of the average of the transformation on the
-- signals
logAverageSig :: [[Int]] -> Int -> (VU.Vector Int -> Int -> VU.Vector Double) -> [Double]
logAverageSig signals ms f = log_avg_f_sig
    where log_avg_f_sig = map log $ averageSig signals ms f -- Take log of signals

