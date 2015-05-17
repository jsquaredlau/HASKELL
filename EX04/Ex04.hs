module Ex04 where

import Data.Char
import System.Environment
import System.IO

capitalise :: FilePath -> FilePath -> IO ()
capitalise fromPath toPath = do
  contents <- readFile fromPath
  writeFile toPath (map toUpper contents)

sumFile :: IO ()
sumFile = do
  args <- getArgs
  contents <- readFile (args !! 0)
  writeFile (args !! 1) (show (sum (map read (lines contents))))
