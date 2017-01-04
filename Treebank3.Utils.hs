module TreeBank3.Utils (getFiles) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Data.List

--- based on Real World Haskell, Ch. 9
getFiles :: FilePath -> IO [FilePath]
getFiles basedir = do
  names <- getDirectoryContents basedir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
                    let path = basedir </> name
                    isDirectory <- doesDirectoryExist path
                    if isDirectory
                      then getFiles path
                      else return [path]
  let mrgprd = filter (\f -> (lastN 3 f) `elem` ["mrg", "prd"])
  return (mrgprd (concat paths))

--- concise way to get the last N elements from a string (list).
--- from http://stackoverflow.com/a/17253092
lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)
