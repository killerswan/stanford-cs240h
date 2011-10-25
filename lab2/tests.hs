-- Copyright (c) 2011 Kevin Cantu

module Main where

import HilbertCoordinates
import HilbertCurve
import HilbertRTree
import HilbertCombo
import Control.Monad
import Data.List
import Data.Word
import Test.QuickCheck

--
-- misc.
--

main :: IO ()
main =
   do
      quickCheck hrtQueryIdentity


w16ofLength :: Int -> Gen [Word16]
w16ofLength n = replicateM n arbitrary
{- restricting this to Word16 is weird, given that
-- the strings are parsed as Integer, so I can detect and
-- `error` on overflows of my hilbert curve
-}


--
-- HilbertCombo
--

-- id
hrtQueryIdentity :: Property
hrtQueryIdentity =
   let
      nsToCoord ns = intercalate ", " $ map show ns
      testingThis coord = head . hrtSearchWithCoord coord . hrtFromCoordList $ coord:[]
   in
      forAll (w16ofLength 8) $ \ns -> nsToCoord ns == testingThis (nsToCoord ns)


--
-- HilbertCurve
--

{- If I turned hilbertValue into a Maybe,
 - this bounds checking would be easy to test.
 - Is there a way to test for which conditions call `error`?
hilbertCurveBoundsCheck :: Pt -> Property
hilbertCurveBoundsCheck p@(Pt x y) =
   bound n = 0 <= n && n < 65536
   ...
 -}




{- question: why does lab2 thrown an error when loading
 - both rects.txt and rects2.txt, because of the out-of-bounds
 - values in rects2.txt, BUT NOT throw an error when rects2.txt alone
 - is read?
 - Particularly, this should not happen:

      $ ./cabal-dev/bin/lab2 rects2.txt
      rects2.txt: 2 shapes read in 0.0 milliseconds
      >>> 1,1,65000,65000,1,1,1,1
      found 2 matches in 0.0 microseconds (2135934 cycles):
          5391,7919,5391,7873,5356,7873,-10000,353535535353
          5391,8260,5391,8216,5357,8216,5357,199000

- Am I wrapping the read ... :: [Integer] in some other type?
-}


{- Also, this smells wrong:

      $ ./cabal-dev/bin/lab2 rects.txtrects.txt: 1454 shapes read in 90.0 milliseconds
      >>> 1,1,65535,65535,1,1,1,1
      found 58 matches in 0.0 microseconds (1054590 cycles):
          1965,6375,1965,6350,2509,6350,2509,6375
          3536,6555,3536,6530,3760,6530,3760,6555
          4233,6937,4233,6971,3888,6971,3888,6937
          2844,6464,2844,6440,2992,6440,2992,6464

      >>> 0,0,65536,65536,1,1,1,1
      found 58 matches in 0.0 microseconds (1047033 cycles):
          1965,6375,1965,6350,2509,6350,2509,6375
          3536,6555,3536,6530,3760,6530,3760,6555
          4233,6937,4233,6971,3888,6971,3888,6937
          2844,6464,2844,6440,2992,6440,2992,6464

      >>> 1,1,1,65536,65536,65535,65536,1
      found 58 matches in 0.0 microseconds (1489308 cycles):
          1965,6375,1965,6350,2509,6350,2509,6375
          3536,6555,3536,6530,3760,6530,3760,6555
          4233,6937,4233,6971,3888,6971,3888,6937
          2844,6464,2844,6440,2992,6440,2992,6464

      >>> 

 -}


