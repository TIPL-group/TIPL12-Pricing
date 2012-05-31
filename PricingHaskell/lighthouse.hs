#!/usr/bin/env runhaskell

-- lighthouse.hs     "LIGHTHOUSE" NESTED SAMPLING APPLICATION
-- (GNU General Public License software, (C) Sivia and Skilling 2006)
--              u=0                                 u=1
--               -------------------------------------
--          y=2 |:::::::::::::::::::::::::::::::::::::| v=1
--              |::::::::::::::::::::::LIGHT::::::::::|
--         north|::::::::::::::::::::::HOUSE::::::::::|
--              |:::::::::::::::::::::::::::::::::::::|
--              |:::::::::::::::::::::::::::::::::::::|
--          y=0 |:::::::::::::::::::::::::::::::::::::| v=0
-- --*--------------*----*--------*-**--**--*-*-------------*--------
--             x=-2          coastline -->east      x=2
-- Problem:
--  Lighthouse at (x,y) emitted n flashes observed at D[.] on coast.
-- Inputs:
--  Prior(u)    is uniform (=1) over (0,1), mapped to x = 4*u - 2; and
--  Prior(v)    is uniform (=1) over (0,1), mapped to y = 2*v; so that
--  Position    is 2-dimensional -2 < x < 2, 0 < y < 2 with flat prior
--  Likelihood  is L(x,y) = PRODUCT[k] (y/pi) / ((D[k] - x)^2 + y^2)
-- Outputs:
--  Evidence    is Z = INTEGRAL L(x,y) Prior(x,y) dxdy
--  Posterior   is P(x,y) = L(x,y) / Z estimating lighthouse position
--  Information is H = INTEGRAL P(x,y) log(P(x,y)/Prior(x,y)) dxdy

import qualified Data.Vector.Unboxed as UV
import Control.Monad (mapM)
import Statistics.MiniNest
import System.Random (randomIO)
import Text.Printf

data Lighthouse = Lighthouse {
    lhU :: Double,
    lhV :: Double,
    lhX :: Double,
    lhY :: Double,
    lhLogL :: Double,
    lhLogWt :: Double 
} deriving (Eq, Show)

instance Ord Lighthouse where
    a <= b = lhLogL a <= lhLogL b

instance SamplingObject Lighthouse where
   setLogWt lh newLogWt = lh { lhLogWt = newLogWt }
   getLogWt lh = lhLogWt lh
   getLogL lh = lhLogL lh

logLhoodOfData :: UV.Vector Double -> Double -> Double -> Double
logLhoodOfData observations x y = UV.sum $ UV.map term observations
    where term dk = log (y / pi) - log ((dk - x)*(dk - x) + y*y)

-- logLikelihood function
-- x: Easterly position
-- y: Northerly position
logLhood :: Double -> Double -> Double
logLhood x y = logLhoodOfData lhData x y

lhData = UV.fromList [4.73,  0.45, -1.73,  1.09,  2.19,  0.12, 1.31,
                      1.00,  1.32,  1.07,  0.86, -0.49, -2.59,  1.73,  2.11,
                      1.61,  4.98,  1.71, 2.23,-57.20,  0.96,  1.25, -1.56,
                      2.45, 1.19,  2.17,-10.66,  1.91, -4.16, 1.92,  0.10,  1.98,
                      -2.51, 5.55, -0.47,  1.91,  0.95, -0.78, -0.84,  1.72,
                      -0.01,  1.48, 2.70,  1.21,  4.41, -4.79,  1.33,  0.81,
                      0.20,  1.58,  1.29, 16.19,  2.75, -2.38, -1.79,
                      6.50,-18.53,  0.72,  0.94,  3.64, 1.94, -0.11, 1.57,  0.57]

-- |Sample from U[0,1]
uniform :: IO Double
uniform = randomIO

sampleFromPrior :: IO Lighthouse
sampleFromPrior = do
    u <- uniform
    v <- uniform
    let x=4*u - 2
        y=2*v
    return $ Lighthouse u v x y (logLhood x y) 0

-- |Evolve Lighthouse within likelihood constraint
-- obj: Lighthouse being evolved
-- logLstar: Likelihood constraint L > Lstar
explore :: Lighthouse -> Double -> IO Lighthouse
explore obj logLstar =
    explore' step m accept reject (lhU obj) (lhV obj) (lhX obj) (lhY obj)
        (lhLogL obj)
    where step = 0.1      -- Initial guess suitable step-size in (0,1)
          m = 20          -- MCMC counter (pre-judged # steps)
          accept = 0      -- # MCMC acceptances
          reject = 0      -- # MCMC rejections
          explore' step m accept reject u v x y logL = do
            -- Trial Lighthouse
            unif1 <- uniform
            unif2 <- uniform
            let u' = wrapAround $ u + step * (2*unif1 - 1)  -- |move| < step
                v' = wrapAround $ v + step * (2*unif2 - 1)  -- |move| < step
                x' = 4*u' - 2    -- map to x
                y' = 2*v'        -- map to y
                logL' = logLhood x' y'

            -- Accept if and only if within hard likelihood constraint
            obj' <- 
                if logL' > logLstar
                    then return $ Lighthouse u' v' x' y' logL' (lhLogWt obj)
                    else return $ Lighthouse u v x y logL (lhLogWt obj)
            (accept, reject) <- if logL' > logLstar
                                    then return (accept + 1, reject)
                                    else return (accept, reject + 1)
            
            -- Refine step-size to let acceptance ratio converge around 50%
            step <- if accept > reject
                        then return $ step * exp(1.0 / accept)
                        else return step
            step <- if accept < reject
                        then return $ step / exp(1.0 / reject)
                        else return step
            if m == 0
                then return obj'
                else explore' step (m-1) accept reject (lhU obj') (lhV obj')
                        (lhX obj') (lhY obj') (lhLogL obj')

wrapAround :: Double -> Double
wrapAround x = x - (fromIntegral $ floor x)

data Stats = Stats { meanX :: Double,
                     meanY :: Double,
                     stddevX :: Double,
                     stddevY :: Double }

instance Show Stats where
    show s = (printf "x = %.2f +- %.2f\n" (meanX s) (stddevX s) ++
              printf "y = %.2f +- %.2f\n" (meanY s) (stddevY s))

-- Posterior properties, here mean and stddev of x,y
-- Args:
--  samples: Objects defining posterior
--  logZ: Evidence (= total weight = SUM[Samples] Weight)
getStats :: [Lighthouse] -> Double -> Stats
getStats samples logZ =
    Stats {meanX=x,
           meanY=y,
           stddevX=sqrt $ xx - x*x,
           stddevY=sqrt $ yy - y*y }
    where weightsSamples = [(exp (lhLogWt s - logZ), s) | s <- samples]
          x = sum [w*(lhX s) | (w,s) <- weightsSamples]
          y = sum [w*(lhY s) | (w,s) <- weightsSamples]
          xx = sum [w*(lhX s)^2 | (w,s) <- weightsSamples]
          yy = sum [w*(lhY s)^2 | (w,s) <- weightsSamples]

main = do
    let n = 100                -- # number of candidate lighthouses
    let maxIterations = 1000   -- # iterates
    priorSamples <- mapM (\_ -> sampleFromPrior) [1..n]         
    result <- nestedSampling priorSamples explore maxIterations
    let stats = getStats (nsSamples result) (nsLogZ result) 
    print result
    print stats

