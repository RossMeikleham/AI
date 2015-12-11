module AI.Lab2.Lab2 where

import System.Random
import Data.List
import qualified Data.Vector.Unboxed as VU

-- Sample Data either involves speech
-- or is silent
data SoundType = Speech | Silence
    deriving (Eq, Show)

-- Type alias for 
type SampleData = ([Double], SoundType)


loss :: SoundType -> SoundType -> Double
loss expected actual 
    | expected == actual = 0
    | otherwise          = 1


-- Calculate mean of a given feature
mean :: [Double] -> Double
mean [] = 0
mean xs = (sum xs) / (fromIntegral $ length xs)


-- Calculate variance of a given feature
variance :: [Double] -> Double
variance [] = 0
variance xs = (1 / sz) * (sum $ map (\x -> (x - mu)^2) xs)  
    where 
          sz :: Double
          sz = fromIntegral $ length xs
          
          mu :: Double
          mu = mean xs



-- Computes univariate gaussian from given mean and variance
univariateGaussian :: Double -> Double -> Double -> Double
univariateGaussian mu var x = right / left
    where left = sqrt (2 * pi * var)
          right = exp $ - ((x - mu)^2 / (2 * var))
       

-- Calculates gaussian discriminant for univariate gaussian
gaussianDiscriminant :: Double -> Double -> Double -> Double
gaussianDiscriminant mu var x = - log(sqrt(2 * pi * var)) -
                                ((x - mu)^2 / (2 * var)) +
                                log(univariateGaussian mu var x)



-- Calculates the log of the discriminant function 
-- for a given SoundType decision.
probability :: SoundType -> [SampleData] -> ([Double] -> Double)
probability st trainData values = sum predictors
    where
         predictors :: [Double]
         predictors = map (\(m, v, vals) -> 
                        gaussianDiscriminant m v vals) $ zip3 means vars values
         
         means :: [Double]
         means = map mean classSamples

         vars :: [Double]
         vars = map variance classSamples
         
         -- Get the sample data which is of the given SoundType
         classSamples :: [[Double]]
         classSamples = transpose $ map fst $ 
                            filter (\(values, st') -> st == st') trainData


-- Calculate average loss over the test set after training gaussian
-- discriminant functions with the train set.
averageLoss :: [SampleData] -> [SampleData] -> Double
averageLoss trainData testData = 
    (sum $ map (uncurry loss . predict) testData) / (
                fromIntegral $ length testData)

    where
        classPredictors :: [(SoundType, [Double] -> Double)]
        classPredictors = map (\st -> (st, probability st trainData)) 
                                [Speech, Silence]

        predict :: SampleData -> (SoundType, SoundType)
        predict (features, st) = (predictSt, st)
          
          -- Select the class with the largest discriminant
          where (predictSt, _) = 
                  foldl1 (\(st1, p1) (st2, p2) -> if p2 > p1
                                                  then (st2, p2)
                                                  else (st1, p1)
                       ) $ 
                       map (\(st', predictFn) -> 
                            (st', predictFn features)) classPredictors 


-- Calculate Average loss using cross validation given 
-- all test/training sets
cvAverageLoss :: [([SampleData], [SampleData])] -> Double
cvAverageLoss sets = 
    (sum $ map (\(trainData, testData) -> 
            averageLoss trainData testData) sets) /
                (fromIntegral $ length sets)


-- Given a list of signals, a window length and a function which
-- takes a list of signals and that window lenth and transforms the 
-- signal; this function takes the average of the transformation on 
-- the signals
averageSig :: [[Int]] -> Int -> (VU.Vector Int -> Int -> 
                                    VU.Vector Double) -> [Double]
averageSig signals ms f = avg_f_signals
    where f_signals = 
            map (\m -> f (VU.fromList m) 
                ms ) signals -- Apply transformation
         
          sum_f_signals = 
            map (VU.foldl' (+) 0) f_signals -- Sum signals
         
          avg_f_signals = 
            map (/ (fromIntegral $ length signals)) 
                sum_f_signals -- Average signals


-- Given a list of list of signals, a window length and a function which
-- takes a list of signals and that window lenth and transforms the signal;
-- this function takes the log of the average of the transformation on the
-- signals
logAverageSig :: [[Int]] -> Int ->  
        (VU.Vector Int -> Int -> VU.Vector Double) -> [Double]

logAverageSig signals ms f = log_avg_f_sig
    where log_avg_f_sig = map log $ averageSig signals ms f 


-- Removes kth element from given list, throws an error
-- if the size of the list is >= k
removeK :: [a] -> Int -> (a, [a])
removeK xs = removeK' [] (head xs) (tail xs)
    where removeK' :: [a] -> a -> [a] -> Int -> (a, [a])
          removeK' begin elem end 0 = (elem, begin ++ end)
          removeK' begin elem end k = 
            removeK' (begin ++ [elem]) (head end) (tail end) (k -1)


-- Attempts to remove k random elements from a given list,
-- returns a tuple containing the list of removed elements and
-- the elements left remaining in the list
-- if total elements <= k then the list itself is returned as
-- the first tuple value
removeRandK :: [a] -> Int -> IO ([a], [a])
removeRandK xs = removeRandK' ([], xs)
    where 
          removeRandK' :: ([a], [a]) -> Int -> IO ([a], [a])
          removeRandK' (rnds, []) _ = return (rnds, [])
          removeRandK' (rnds, xs)  0 = return (rnds, xs)
          removeRandK' (rnds, xs) k = do
                n <- randomRIO (0, length xs - 1)
                let (elem, xs') = removeK xs n 
                removeRandK' (elem : rnds, xs') (k - 1)


-- Splits data into k uniformly sized random subsets 
-- requires that k divides the length of the 
-- given data, and that the length of data is > 0 
-- otherwise an error is thrown
splitK :: [a] -> Int -> IO [[a]]
splitK [] _ = error "splitK: given list is empty"
splitK xs k = 
    if length xs `mod` k /= 0 
        then error $ "splitK: " ++ show k ++ " does not divide " ++ 
                        show (length xs)

        else splitK' ([], xs) k

    where 
          splitK' :: ([[a]], [a]) -> Int -> IO [[a]]
          splitK' (splits, []) 0 = return splits
          splitK' (splits, xs) k = do
            (set, leftOver) <- removeRandK xs (length xs `div` k) 
            splitK' (splits ++ [set], leftOver) (k - 1)


-- From partitioned data obtain all training and test sets
getTrainTest :: [[a]] -> [([a], [a])]
getTrainTest xs = 
    map (\k -> 
        let (test, train) = removeK xs k 
            in (concat train, test)) [0 .. length xs - 1]
