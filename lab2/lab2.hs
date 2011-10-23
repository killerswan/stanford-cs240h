-- Copyright (c) 2011 Kevin Cantu

module Main where

import Prelude hiding (catch)
import Control.Exception
import Control.Monad (forever) -- (when)
import qualified Data.ByteString.Lazy as B
import Data.List
{-
import qualified Data.Text.Lazy.IO as TIO
-}
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import HilbertCoordinates  -- lab2
import HilbertCurve        -- lab2
import HilbertRTree        -- lab2
import System.Console.GetOpt
import System.CPUTime
import System.CPUTime.Rdtsc
import System.Environment
import System.Exit
import System.IO
import Text.Printf



coordsToRect :: String -> MBR
coordsToRect coords =
   let
      -- read the coordinates as an array
      coords' = if length flatArray == 8
                then unfoldr f flatArray
                else error "I was expecting 4 points..."
              where
                  flatArray = read ("[" ++ coords ++ "]") :: [Integer]
                  f [] = Nothing
                  f xs = case length xs `mod` 2 of
                           0 -> Just (tup $ splitAt 2 xs)
                           _ -> error "this object path is junk"
                  tup (xy, r) = (Pt (xy !! 0) (xy !! 1), r)


      -- function to calculate bounding rectangle
      -- of many points
      bounding :: [Pt] -> MBR
      bounding [] = error "there is no bounding rectangle of an empty coordinate"
      bounding cs@(c:_) = foldl maxR (toRect c) cs
                       where
                           toRect (Pt x y) = MBR (Pt x y) (Pt x y)
                           maxR (MBR (Pt minx miny) (Pt maxx maxy)) (Pt x y) =
                              let
                                 minx' = min minx x
                                 miny' = min miny y
                                 maxx' = max maxx x
                                 maxy' = max maxy y
                              in 
                                 MBR (Pt minx' miny') (Pt maxx' maxy')
   in
      bounding coords'


coordsToInfo :: String -> Tree
coordsToInfo coords =
   let
      rect = coordsToRect coords

      center :: MBR -> Pt
      center (MBR (Pt x y) (Pt x' y')) = Pt (div (x+x') 2) (div (y+y') 2)

      h = LHV $ hilbertValue $ center rect
   in
      Info rect h coords
                           
   
-- converting between units of time
toMs :: Integer -> Double
toMs ps = fromIntegral ps / fromIntegral ((10 :: Integer)^(9 :: Integer))

toUs :: Integer -> Double
toUs ps = fromIntegral ps / fromIntegral ((10 :: Integer)^(6 :: Integer))

toPs :: Integer -> Double
toPs = fromIntegral





data MyOptions = MyOptions { }

options :: [ OptDescr (MyOptions -> IO MyOptions) ]
options =
   [ Option "h" ["help"] 
         (NoArg  $ \_ ->
            do
               prg <- getProgName
               let header = "Usage: " ++ prg ++ " [file1 file2 ...]"
               hPutStrLn stderr $ usageInfo header options
               exitWith ExitSuccess)
         "display this help"
   ]

main :: IO ()
main = 
   do 
      args <- getArgs
      let (actions, nonOptions, _) = getOpt Permute options args
      _ <- foldl (>>=) (return MyOptions {}) actions

      let files = nonOptions

      contents <- mapM B.readFile files

      -- read and insert each line of coordinates
      let coordList = map T.unpack . T.lines . TE.decodeUtf8 . B.concat $ contents :: [String]
      let insertCoords = insertHRT . coordsToInfo

{-
      -- coordinate testing
      let x = insertCoords "3,3, 4,5, 5,7, 98,9" NewHRT
      let x' = insertCoords "9,7, 78,9, 9,5000, 899,3444" x
      let x'' = insertCoords "3453,5345, 789,9790, 770,988, 8234,6000" x'
      let x''' = insertCoords "304,28340, 3450,534, 6000,27, 3450,3453" x''
-}


      -- START
      time0 <- getCPUTime      
      hrt   <- evaluate $ foldl (flip insertCoords) NewHRT coordList
      time1 <- getCPUTime      
      -- STOP

      let delta = printf "%0.1f milliseconds" . toMs $ time1 - time0
      putStrLn $ intercalate ", " files ++ ": " ++ show (length coordList) ++ " shapes read in " ++ delta


      forever $ do

         putStr ">>> "
         hFlush stdout -- seriously?
         query <- getLine

         let
            handler :: ErrorCall -> IO ()
            handler e = putStrLn $ "error: " ++ show e

         runQuery query hrt `catch` handler


runQuery :: String -> HilbertRTree -> IO ()
runQuery query hrt =
   let
      getStr (Info _ _ str) = str
      getStr _ = error "what did we return, a Branch or Leaf?!"

      take' n xs = (length xs, take n xs)
      (numberFound, found) = take' 4 . searchHRT (coordsToRect query) $ hrt
   in
   do
      -- START
      time0 <- getCPUTime
      cyc0  <- rdtsc
      resultsToDisplay <- evaluate $ map getStr found
      cyc1  <- rdtsc
      time1 <- getCPUTime
      -- STOP

      let delta = printf "%0f microseconds (%d cycles)" (toUs $ time1 - time0)
                                                        (fromIntegral (cyc1-cyc0) :: Int)

      putStrLn $ "found " ++ show numberFound ++ " matches in " ++ delta ++ ":"
      mapM_ (\ss -> putStrLn $ "    " ++ ss) resultsToDisplay
      putStrLn ""


