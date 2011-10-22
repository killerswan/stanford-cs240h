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


-- Hilbert H Tree
data Tree = Branch R H [Tree]  -- contains branches and leaves
          | Leaf   R H [Tree]  -- contains only infos
          | Info   R H Details -- 
          deriving (Show, Ord, Eq)




leafIsNotFull :: [Tree] -> Bool
leafIsNotFull s = length s < 3

branchIsNotFull :: [Tree] -> Bool
branchIsNotFull s = length s < 3


-- big insert function
--
insertT :: Tree -- Info
        -> Tree -- Existing Leaf or Branch
        -> Maybe Tree

insertT _ (Info _ _ _) = error "sorry, inserting into an Info makes no sense"

insertT newi (Leaf _ _ infos) =
   if leafIsNotFull infos
   then Just $ children2Tree $ insert newi infos
   else Nothing

insertT newi (Branch _ _ trees) =
   let
      -- yes, this is a zipper... TODO
      (smallerTs, targetT, biggerTs) =
         let
            lessThanH (Info _ h' _) (Branch _ h'' _) = h' > h''
            lessThanH (Info _ h' _) (Leaf   _ h'' _) = h' > h''
            lessThanH _ _ = error "i am confused: what are you looking for?"

            (smallerH, biggerH) = span (lessThanH newi) trees
         in
            case (reverse smallerH, biggerH) of
               (sss, t:ts) -> (sss, t, ts)
               (s:ss, tts) -> (reverse ss, s, tts)
               (_, _)      -> error "well, this is an empty list..."

      siblings = smallerTs -- TODO: think about this

      siblingsNotFull s =
         let
            notFull (Branch _ _ xs) = branchIsNotFull xs
            notFull (Leaf _ _ xs)   = leafIsNotFull xs
            notFull (Info _ _ _)    = error "an Info can't be full or empty"
         in
            elem True . map notFull $ s

      addEmptySibling bs@(Branch _ _ _:_) = Branch r0 h0 [] : bs
      addEmptySibling ls@(Leaf   _ _ _:_) = Leaf   r0 h0 [] : ls
      addEmptySibling _ = error "sorry, new empty siblings of Info make no sense"

   in
      -- recursive call
      case trees of
         [] -> Nothing
               -- insertT (Info r0 h0 "wtf") (Branch r0 h0 [])

         _  ->
            case insertT newi targetT of
               Just t  -> Just $ children2Tree $ concat [smallerTs, [t], biggerTs]

                          -- insertT (Info r0 h0 "wtf") (Leaf r0 h0 [])
                          -- insertT (Info r0 h0 "wtf") (Branch r0 h0 [Leaf r0 h0 []])

               Nothing ->
                  case siblingsNotFull siblings of
                     True  -> insertT newi . children2Tree $ shuffle siblings
                     False ->
                        case branchIsNotFull trees of
                           True  -> -- question: how can I guarantee this will give
                                    --           my target tree free space for the recursive call?
                                    --

                                    insertT newi . children2Tree . shuffle $ addEmptySibling trees

                                    --  insertT (Info r0 h0 "wtf")
                                    --          (Branch r0 h0
                                    --             [Leaf r0 h0
                                    --                [Info r0 h0 "existing 0",
                                    --                 Info r0 h0 "existing 1",
                                    --                 Info r0 h0 "existing 2"]])

                           False -> Nothing


trees2children :: [Tree] -- parents
                        -> [Tree] -- children
trees2children parents = unwrap' =<< parents
                    where
                       unwrap' (Branch _ _ children) = children
                       unwrap' (Leaf _ _ children) = children
                       unwrap' (Info _ _ _) = error "unwrapping an info makes no sense"


-- this seems ugly
collectR :: [Tree] -> MBR
collectR cs =
   let
      f r' (Info   r _ _) = maxRect r r'
      f r' (Leaf   r _ _) = maxRect r r'
      f r' (Branch r _ _) = maxRect r r'

      maxRect (MBR (Pt x0 x1) (Pt y0 y1))
              (MBR (Pt x0' x1') (Pt y0' y1')) = MBR (Pt (min x0 x0') (min x1 x1'))
                                                    (Pt (max y0 y0') (max y1 y1'))
   in
      foldl f r0 cs


collectH :: [Tree] -> LHV
collectH cs = foldl f h0 cs
            where
               f h' (Info   _ h _) = max h h'
               f h' (Leaf   _ h _) = max h h'
               f h' (Branch _ h _) = max h h'


children2Tree :: [Tree] -> Tree
children2Tree children =
   let
      f cs@(Info   _ _ _ : _) = Leaf   r h cs
      f cs@(Leaf   _ _ _ : _) = Branch r h cs
      f cs@(Branch _ _ _ : _) = Branch r h cs
      f [] = error "empty tree makes no sense here..."

      r = collectR children
      h = collectH children
   in
      f children



shuffle :: [Tree] -- parents to shuffle
        -> [Tree]
shuffle trees =
   let
      children = trees2children trees
      trees' = unfoldr f children
             where
               len = length children `div` length trees
               f [] = Nothing
               f xs = Just (splitAt len xs)
   in
      map children2Tree trees'
      {- test:
      shuffle [Leaf r0 h0 [], 
               Leaf r0 h0 [Info (MBR (Pt 15 18) (Pt 37 29)) (LHV 67) "zzzzz",
                           Info r0 (LHV 90) "aaa"],
               Leaf r0 (LHV 9000) [Info r0 (LHV 700) "bbbbb"]]
      -}



r0 :: MBR
r0 = MBR (Pt 0 0) (Pt 0 0)
h0 :: LHV
h0 = LHV 0




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


