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
import HilbertCurve
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO





--data Pt  = Pt  Integer Integer deriving (Show, Ord, Eq)
data MBR = MBR Pt Pt           deriving (Show, Ord, Eq)
data LHV = LHV Integer         deriving (Show, Ord, Eq)

data LeafNode = LeafNode { ln_mbr  :: MBR
                         , ln_lhv  :: LHV
                         , ln_info :: String
                         } deriving (Show, Eq)

instance Ord LeafNode where
   -- this way, we can use `insert` to put each node into the list
   (<=) x y = ln_lhv x <= ln_lhv y

data BranchNode = BranchNode { bn_mbr  :: MBR
                             , bn_lhv  :: LHV
                             , bn_leaf :: Branch
                             } deriving (Show, Eq)

instance Ord BranchNode where
   (<=) x y = bn_lhv x <= bn_lhv y

data Branch = Leaf [LeafNode]
            | Branch [BranchNode] deriving (Show, Ord, Eq)












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


