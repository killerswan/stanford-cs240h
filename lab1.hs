-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental
--
-- Lab 1

--module Main (main) where
module Main where

import qualified Data.ByteString.Lazy as B
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

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

countWordsInFile :: FilePath -> B.ByteString -> IO ()
countWordsInFile path content =
   let
      ws = (T.words . TE.decodeUtf8) content
   in
      sequence_ $ map TIO.putStrLn ws

main :: IO ()
main = 
    do 
        args <- getArgs
        let (actions, nonOptions, _) = getOpt Permute options args
        opts <- foldl (>>=) (return defaultOpts) actions

        fileMapWithPath countWordsInFile nonOptions


