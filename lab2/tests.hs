-- Copyright (c) 2011 Kevin Cantu

module Main where

import HilbertCoordinates
import HilbertCurve
import HilbertRTree
import HilbertCombo
import Data.List
import Data.Word
import Test.QuickCheck

main :: IO ()
main =
   do
      quickCheck hrtIdentity

-- points larger than our default boundary result in errors
-- 0-65536



-- use Word16 to limit our randomly generated ints to 0-65536
-- until i find a better way to make QuickCheck do its job...
hrtIdentity :: [Word16] -> Property
hrtIdentity ns =
   let
      testingThis :: String
      testingThis = head $ hrtIdList $ s:[]

      hrtIdList :: [String] -> [String]
      hrtIdList = hrtSearchWithCoord s . hrtFromCoordList

      s :: String
      s = intercalate ", " $ map show ns
   in
      length ns == 8 ==> s == testingThis



{-
      -- coordinate testing
      let x = insertCoords "3,3, 4,5, 5,7, 98,9" NewHRT
      let x' = insertCoords "9,7, 78,9, 9,5000, 899,3444" x
      let x'' = insertCoords "3453,5345, 789,9790, 770,988, 8234,6000" x'
      let x''' = insertCoords "304,28340, 3450,534, 6000,27, 3450,3453" x''
-}

      {- test:
      shuffle [Leaf r0 h0 [], 
               Leaf r0 h0 [Info (MBR (Pt 15 18) (Pt 37 29)) (LHV 67) "zzzzz",
                           Info r0 (LHV 90) "aaa"],
               Leaf r0 (LHV 9000) [Info r0 (LHV 700) "bbbbb"]]
      -}

               -- insertT (Info r0 h0 "wtf") (Branch r0 h0 [])
                          -- insertT (Info r0 h0 "wtf") (Leaf r0 h0 [])
                          -- insertT (Info r0 h0 "wtf") (Branch r0 h0 [Leaf r0 h0 []])
                                    --  insertT (Info r0 h0 "wtf")
                                    --          (Branch r0 h0
                                    --             [Leaf r0 h0
                                    --                [Info r0 h0 "existing 0",
                                    --                 Info r0 h0 "existing 1",
                                    --                 Info r0 h0 "existing 2"]])




--      hrtFromCoordList coordList
--      hrtSearchWithCoord query hrt


