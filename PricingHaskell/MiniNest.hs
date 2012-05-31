module Statistics.MiniNest where

import Control.Monad (forM)
import Data.IORef
import Data.List (sort)
import System.Random (randomRIO)
import Text.Printf

-- logarithmic addition log(exp(x)+exp(y))
plus :: Double -> Double -> Double
plus x y
     | x > y = x + log (1 + exp (y-x))
     | otherwise = y + log (1 + exp (x-y))

data NestedSamplingResult a = NestedSamplingResult {
    nsLogZ :: Double,
    nsLogZdelta :: Double,  -- evidence +- deviation
    nsInfoNats :: Double,   -- information in nats
    nsSamples :: [a] }

instance Show (NestedSamplingResult a) where
    show result =
        (printf "logZ: %.2f +- %.2f\n" (nsLogZ result) (nsLogZdelta result) ++ 
         printf "information: %.2f nats\n" (nsInfoNats result) ++
         printf "%i samples\n" (length $ nsSamples result))

class SamplingObject a where
   setLogWt :: a -> Double -> a
   getLogWt :: a -> Double
   getLogL :: a -> Double

-- |nestedSampling computes the evidence Z and samples from the posterior.
-- Args:
--   priorSamples: a list of samples from the prior.
--   explore: a function that evolves an object within a likelihood constraint.
--   iterations: number of iterations to run.
nestedSampling :: (Ord a, SamplingObject a) => [a] -> (a -> Double -> IO a) -> Int -> IO (NestedSamplingResult a)
nestedSampling priorSamples explore iterations = do
    let n = length priorSamples

    -- Collection of n objects
    objsRef <- newIORef priorSamples
    samplesRef <- newIORef []              -- Posterior samples
    hRef <- newIORef 0                     -- Information, initially 0
    logZRef <- newIORef (-10**37)          -- ln(Evidence Z, initially 0)

    -- Outermost interval of prior mass
    -- ln(width in prior mass)
    logWidthRef <- newIORef $ getLogWidth n

    -- NESTED SAMPLING LOOP ______________________________________________
    forM [1..iterations] (\nest -> do
        -- Worst object in collection, with Weight = width * Likelihood
        objs <- readIORef objsRef
        let worst = head $ sort objs
        logwidth <- readIORef logWidthRef
        let worst' = setLogWt worst (logwidth + (getLogL worst))

        -- Update Evidence Z and Information H
        logZ <- readIORef logZRef
        h <- readIORef hRef
        let logZnew = plus logZ (getLogWt worst')
        writeIORef hRef $ (exp $ getLogWt worst' - logZnew) * (getLogL worst')
            + (exp $ logZ - logZnew) * (h + logZ) - logZnew
        writeIORef logZRef logZnew

        -- Posterior Samples (optional)
        oldSamples <- readIORef samplesRef
        writeIORef samplesRef (worst' : oldSamples)

        -- Kill worst object.
        let objs' = drop 1 $ sort objs
        writeIORef objsRef objs'

        -- Copy another object at random.
        objToCopy <- choice objs'

        -- new likelihood constraint
        let logLstar = getLogL worst'

        -- Evolve copied object within constraint
        mutatedCopy <- explore objToCopy logLstar

        -- Save copied and mutated object.
        writeIORef objsRef (mutatedCopy : objs')

        -- Shrink interval
        writeIORef logWidthRef (logwidth - 1.0 / fromIntegral n))

    -- Exit with evidence Z, information H, and optional posterior Samples
    logZ <- readIORef logZRef
    h <- readIORef hRef
    samples <- readIORef samplesRef
    return $ NestedSamplingResult {
        nsLogZ=logZ,
        nsLogZdelta=sqrt (h / fromIntegral n),  -- evidence +- deviation
        nsInfoNats=h,                           -- information in nats
        nsSamples=samples
    }

-- |choice chooses uniformly at random from a list.
choice :: [a] -> IO a
choice [] = error "No items specified for choice."
choice [x] = return x
choice xs = do
    let n = length xs
    k <- randomRIO (0, n-1)
    return $ xs !! k

floatRatio :: Int -> Int -> Float
floatRatio n1 n2 = fromIntegral n1 / fromIntegral n2

getLogWidth :: Int -> Double
getLogWidth n = log $ 1.0 - exp(-1.0 / fromIntegral n)

