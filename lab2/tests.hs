-- Copyright (c) 2011 Kevin Cantu

module Main where

import HilbertCoordinates
import HilbertCurve
import HilbertRTree
import HilbertCombo
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
      quickCheck hrtQueryIdentityMany




--
-- HilbertCombo
--

hrtQueryIdentity :: Property
hrtQueryIdentity =
   let
      nsToCoord ns = intercalate ", " $ map show ns
      testingThis coord = head . hrtSearchWithCoord coord . hrtFromCoordList $ coord:[]
   in
      forAll (vector 8 :: Gen [Word16]) $ \ns -> nsToCoord ns == testingThis (nsToCoord ns)
{- restricting this to Word16 is weird, given that
-- the strings are parsed as Integer, so I can detect and
-- `error` on overflows of my hilbert curve
-}


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


-- make a lot of coordinates,
-- put them in a tree,
-- then see if each one can be found in the combined tree
hrtQueryIdentityMany :: Property
hrtQueryIdentityMany =
   let
      someLists = listOf1 (vector 8 :: Gen [Word16])
   in
      -- there has got to be a better way than using forAll
      forAll someLists $ \ls -> 
         let
            coords lists = map (intercalate ", " . map show) lists
            
            coords' :: [String]
            coords' = coords ls

            hrt0 :: HilbertRTree
            hrt0 = hrtFromCoordList coords'

            searchAndFilter :: String -> [String]
            searchAndFilter cs = filter (==cs) $ hrtSearchWithCoord cs hrt0
         in
            sort coords' == sort (searchAndFilter =<< coords')


