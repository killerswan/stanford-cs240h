-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

{-# LANGUAGE DoAndIfThenElse #-}  -- also considering: OverloadedStrings

module Main where

import Control.Monad (when)
import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO


data Options = Options { }


defaultOpts :: Options
defaultOpts = Options { }


hugeWordLim :: Int64
hugeWordLim = 80 * 75 `div` 100


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


-- apply a function (which uses the path) to a list of files and/or stdin
fileMapWithPath :: ( FilePath -> B.ByteString -> IO () ) -> [FilePath] -> IO ()
fileMapWithPath f paths = 
    let    
        -- apply f to stdin
        stdinF :: IO ()
        stdinF = B.getContents >>= f "-"

        -- apply f to a file (or stdin, when "-")
        fileF :: FilePath -> IO ()
        fileF "-"  = stdinF
        fileF path = (B.readFile path >>= f path) `catch` (hPutStrLn stderr . show )
    in
        case paths of
            [] -> stdinF
            _  -> mapM_ fileF paths 


-- count and then display word frequencies
countAndDisplay :: FilePath -> B.ByteString -> IO ()
countAndDisplay path content =
   let
      -- get the words and counts
      wordCounts = countWordsInFile content

      -- as a list
      sortedWordCounts =
         let
            -- highest frequencies first
            sortFunc (_,a) (_,b) | a > b     = LT
                                 | a < b     = GT
                                 | otherwise = EQ
         in
            sortBy sortFunc $ M.toList wordCounts

      -- length of longest word
      maxWordLen =
         let
            -- longest word length so far
            longest :: Int64 -> T.Text -> Int64
            longest oldLen key | len > hugeWordLim = oldLen
                               | len > oldLen      = len
                               | otherwise         = oldLen
                               where len = T.length key
         in
            foldl longest 0 $ M.keys wordCounts

      -- count of most frequent word
      maxFreq =
         let
            greaterOf f f' = if f' > f then f' else f
         in
            foldl greaterOf 0 $ M.elems wordCounts
   in
      do
         -- header
         TIO.putStrLn $ T.pack $ "Statisitics for: " ++ path

         -- print all words
         mapM_ (display maxWordLen maxFreq) sortedWordCounts

         -- blank line
         TIO.putStrLn $ T.pack ""


-- count up all the matches of words we find
countWordsInFile :: B.ByteString -> Map T.Text Integer
countWordsInFile content =
   let
      ws = (T.words . TE.decodeUtf8) content

      insertCount :: Map T.Text Integer -> T.Text -> Map T.Text Integer
      insertCount mm w = M.insertWith (+) w 1 mm
   in
      foldl insertCount (M.fromList []) ws


-- display one word we found matches for
display :: Int64 -> Integer -> (T.Text, Integer) -> IO ()
display maxWordLen maxFrequency (word, freq) =
   let
      -- pad the word so all the bars line up
      klen = T.length word
      pad  = T.replicate (maxWordLen + 1 - klen) $ T.pack " "
      
      -- scale things to 80 chars per line
      fullbar   = 80 - maxWordLen - 1
      scaledbar = fromIntegral fullbar * freq `div` maxFrequency
      bar = T.replicate (fromIntegral scaledbar) $ T.pack "#"

   in
      when (scaledbar > 0) $
         if klen <= hugeWordLim
         then
            -- word    ##############
            -- another ######
            TIO.putStrLn $ T.concat [word, pad, bar]
         else
            do
               -- superlongword (not graphed: 4 occurrences)
               TIO.putStr word
               TIO.putStrLn $ T.pack $ " (not graphed: " ++ show freq ++ " occurrences)"


main :: IO ()
main = 
    do 
        args <- getArgs
        let (actions, nonOptions, _) = getOpt Permute options args
        _ <- foldl (>>=) (return defaultOpts) actions  -- ignore the options returned

        fileMapWithPath countAndDisplay nonOptions


