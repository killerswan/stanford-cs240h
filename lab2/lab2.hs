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

-- rename these three later
type R = MBR
type H = LHV
type Details = String

data Tree = Branch R H [Tree]  -- contains branches and leaves
          | Leaf   R H [Tree]  -- contains only infos
          | Info   R H Details -- 
          deriving (Show, Ord, Eq)


leafIsFull :: [Tree] -> Bool
leafIsFull is = length is >= 3

branchIsFull :: [Tree] -> Bool
branchIsFull is = length is >= 3


-- big insert function
--
insertT :: Tree -- Info
        -> Tree -- Existing Leaf or Branch
        -> Maybe Tree

insertT _ (Info _ _ _) = error "sorry, inserting into an Info makes no sense"

insertT newi (Leaf r h infos) =
   if leafIsFull infos
   then Nothing
   else Just (Leaf r0 h0 infos')
      where
         infos' = insert newi infos

insertT newi (Branch r h trees) =
   let
      -- yes, this is a zipper... TODO
      (smallerTs, targetT, biggerTs) =
         let
            lessThanH (Info _ h' _) (Branch _ h'' _) = h'' < h'
            lessThanH (Info _ h' _) (Leaf   _ h'' _) = h'' < h'
            lessThanH _ _ = error "i am confused: what are you looking for?"

            smallerH' = reverse smallerH

            (smallerH,biggerH) = span (lessThanH newi) trees
         in
            case biggerH of
               t:ts -> (smallerH, t, ts)
               []   -> (reverse . tail $ smallerH', head . reverse $ smallerH', [])
               where

      siblings = smallerTs -- TODO: think about this
      siblingsFull s =
         let
            full (Branch _ _ xs) = branchIsFull xs
            full (Leaf _ _ xs)   = leafIsFull xs
            full (Info _ _ _)    = error "an Info can't be full or empty"
         in
            elem False . map full $ s
   in
      -- recursive call
      case insertT newi targetT of
         Just t  -> Just $ Branch r0 h0 (concat [smallerTs, [t], biggerTs])
         Nothing -> -- DUMMY
                    Just $ Branch r0 h0 [targetT]
                     --case siblingsFull siblings






r0 :: MBR
r0 = MBR (Pt 0 0) (Pt 0 0)
h0 :: LHV
h0 = LHV 0











{-
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




insertLeafNode :: LeafNode -> Branch -> Branch

insertBranchNode :: BranchNode -> Branch -> Branch
-}










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


