-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

{-# LANGUAGE DoAndIfThenElse, OverloadedStrings #-}

module Main where

--import Control.Monad (when)
import qualified Data.ByteString.Lazy as B
import Data.List
{-
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
-}
import HilbertCoordinates  -- lab2
import HilbertCurve        -- lab2
import HilbertRTree        -- lab2
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO



coordsToInfo :: String -> Tree
coordsToInfo coords =
   let
      -- read the coordinates as an array
      coords' = unfoldr f flatArray
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
                                 maxx' = min maxx x
                                 maxy' = min maxy y
                              in 
                                 MBR (Pt minx' miny') (Pt maxx' maxy')

      center :: MBR -> Pt
      center (MBR (Pt x y) (Pt x' y')) = Pt (div (x+x') 2) (div (y+y') 2)

      rect = bounding coords'
      h = LHV $ hilbert $ center rect
   in
      Info rect h coords
                           
   




data Options = Options { }

defaultOpts :: Options
defaultOpts = Options { }


-- command line description
-- this format is kinda bone headed:
--   [Option short [long] (property setter-function hint) description]
options :: [ OptDescr (Options -> IO Options) ]
options = [ Option "h" ["help"] 
                   (NoArg  $ \_ -> do
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
      _ <- foldl (>>=) (return defaultOpts) actions

      let files = nonOptions

      contents <- mapM B.readFile files
      B.putStr . B.concat $ contents


      let x = insertHRT (Info r0 h0 "omfg") NewHRT
      let x' = insertHRT (Info r0 h0 "omfg") x
      let x'' = insertHRT (Info r0 h0 "omfg") x'
      let x''' = insertHRT (Info r0 h0 "omfg") x''

      putStrLn $ show x
      putStrLn $ show x'
      putStrLn $ show x''
      putStrLn $ show x'''


