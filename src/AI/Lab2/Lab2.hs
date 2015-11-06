{-#Language GADTs #-}
{-#Language ScopedTypeVariables #-}
module AI.Lab2.Lab2 where

import System.Random
import Data.List
import Control.Applicative
import qualified Data.Vector.Unboxed as VU

data SoundType = Speech | Silence

data SampleData a where
    MkSampleData :: a -> a -> a -> SampleData a


-- Functor instance for SampleData
instance Functor SampleData where
    fmap f (MkSampleData e m z) = MkSampleData (f e) (f m) (f z)

-- Applicative Functor instance for SampleData
instance Applicative SampleData where
    pure a = MkSampleData a a a
    MkSampleData f1 f2 f3 <*> MkSampleData e m z = MkSampleData (f1 e) (f2 m) (f3 z)


-- Calculate mean of SampleData
mean :: (Applicative f) => [f Double] -> f Double
mean [] = pure 0
mean xs = meanSampleData
    where sumSampleData = foldl' (\a b -> (+) <$> a <*> b) (pure 0) xs
          meanSampleData = (/ fromIntegral (length xs)) <$> sumSampleData


-- Calculate variance of SampleData
variance :: forall f. (Applicative f) => [f Double] -> f Double
variance [] = pure 0 
variance xs = variance'
          -- sum(i -> N) (xi - mean x) ^2
    where sumVar = foldl' (\a b -> (+) <$> a <*> minMeanSq b) (pure 0) xs
          -- 1/N (sum(i -> N) (xi - mean x) ^2
          variance' = (/ fromIntegral (length xs)) <$> sumVar  

          -- (xi - (mean x)^2
          minMeanSq :: f Double -> f Double
          minMeanSq sd = fmap (^2) $ (-) <$> sd <*> mean xs


-- Generates gaussian discriminant functions given the mean and variance
gaussianDiscriminant :: forall f. (Applicative f) => 
                        f Double -> f Double -> (f Double -> f Double)
gaussianDiscriminant mu var = univariateGaussian
    where    
        
        -- Generate Univatiate Gaussian Function from given means and
        -- variances from Sample Data
        univariateGaussian :: f Double -> f Double
        univariateGaussian x = (/) <$> left <*> right
            where 
                  left = sqrt <$> ((\v -> 2 * pi * sqrt v) <$> var)  -- sqrt (2 * pi * sigma)
                  right = exp <$> ( (/) <$> rightNum <*> rightDenom) 
                  xMinMean = (-) <$> x <*> mu -- (x - mu)
                  rightNum = (\v -> - (v^2)) <$> xMinMean -- - (x - mu)^2
                  rightDenom = (\v -> 2 * v^2) <$> var -- 2 * sigma^2


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
                n <- randomRIO (0, length xs)
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
        then error $ "splitK: " ++ show k ++ " does not divide " ++ show (length xs)
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




