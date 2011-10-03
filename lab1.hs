-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

{-# LANGUAGE DoAndIfThenElse #-}

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

-- apply a function to a list of files and/or stdin
fileMap :: ( B.ByteString -> IO () ) -> [FilePath] -> IO ()
fileMap f paths = fileMapWithPath (\_ -> f) paths

hugeWordLim :: Int64
hugeWordLim = (80 * 75 `div` 100)

countWordsInFile :: FilePath -> B.ByteString -> IO ()
countWordsInFile path content =
   let
      ws = (T.words . TE.decodeUtf8) content

      insertCount :: Map T.Text Integer -> T.Text -> Map T.Text Integer
      insertCount mm w =
         M.insertWith (+) w 1 mm

      wordCounts = foldl insertCount (M.fromList []) ws

      -- align histogram to this column
      maxKey keys =
         let
            longest :: Int64 -> T.Text -> Int64
            longest oldLen key =
               let
                  len = T.length key
               in
                  if len > hugeWordLim
                  then oldLen
                  else if len > oldLen
                       then len
                       else oldLen
         in
            foldl longest 0 keys

      -- abclhalsdkjhfaksdjhfalskdjhflaiwueykajndkyivcuzyiueyrwe,mnzcvzsdoifwyeiruwyejfhzsdncvzsdhfriuweyriwuehrfkenzm,cnzkjehwuiyeriwueyhriuwhenm,nzfkhwieuyrwiueh,menwi
   
      maxF fs = foldl (\f f' -> if f' > f then f' else f) 0 fs

      maxWordLen = maxKey $ M.keys wordCounts

      maxFreq    = maxF $ M.elems wordCounts

      sortFunc (_,a) (_,b) = if a > b
                            then LT
                            else if a < b
                                 then GT
                                 else EQ

   in
      do
         TIO.putStrLn $ T.pack $ "Statisitics for: " ++ path
         
         let sortedCounts = sortBy sortFunc $ M.toList wordCounts
         sequence_ $ map (display maxWordLen maxFreq) sortedCounts
         TIO.putStrLn $ T.pack ""

display :: Int64 -> Integer -> (T.Text, Integer) -> IO ()
display maxWordLen maxFrequency (word, freq) =
   let
      klen = T.length word
      pad  = T.replicate (maxWordLen + 1 - klen) $ T.pack " "
      
      -- if scaled to 80 per line
      fullbar   = 80 - maxWordLen - 1
      scaledbar = ((fromIntegral fullbar) * freq) `div` maxFrequency
      bar = T.replicate (fromIntegral scaledbar) $ T.pack "#"

   in
      do 
         when (scaledbar > 0) $ do
            if klen <= hugeWordLim
            then
               TIO.putStrLn $ T.concat [word, pad, bar]
            else
               do
                  TIO.putStr word
                  TIO.putStrLn $ T.pack $ " (not graphed: " ++ show freq ++ " occurrences)"

main :: IO ()
main = 
    do 
        args <- getArgs
        let (actions, nonOptions, _) = getOpt Permute options args
        _ <- foldl (>>=) (return defaultOpts) actions  -- ignore the options returned

        fileMapWithPath countWordsInFile nonOptions


