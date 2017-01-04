module Treebank3 (parseFile, searchLabel) where

import NLP.PennTreebank
import Utils.Dir
import Data.Tree
import Data.Tree.Zipper
import Data.String.Utils
import Text.ParserCombinators.Parsec hiding (label)
import System.IO
import Control.DeepSeq

type Label = String

-- Wrapper around the parser, calling it as many times as possible on
-- a string, and returning a list.
parseText :: String -> String -> [Tree String]
parseText path text = case parse (many parseTree) "wsj" (strip text) of
                        Left err -> error $ "Input:\n" ++ show path ++
                                    "\nError:\n" ++ show err
                        Right result -> result

-- Send the content of a file to parseText
parseFile :: FilePath -> IO [Tree String]
parseFile f = withFile f ReadMode $ \handle -> do
                text <- hGetContents handle
                return $!! parseText f (strip text)

-- traverses the whole database, searching for subtrees with a certain label
searchLabel :: [Tree String] -> Label -> [Tree String]
searchLabel trees l = concat (map (queryLabel l) trees)

buildForest :: FilePath -> IO [Tree String]
buildForest basedir = do
  trees <- mapM parseFile =<< getFiles basedir
  return (concat trees)
  
-- search function to get all subtrees with a certain label
queryLabel :: Label -> Tree String -> [Tree String]
queryLabel l tr = filter (eqLabel l) $ subTrees tr

-- helper for the search function
eqLabel :: Label -> Tree String -> Bool
eqLabel l tr = rootLabel tr == l
               
treeToString :: Tree String -> String
treeToString = unwords . concat . (map flatten) . terminals

terminals :: Tree String -> [Tree String]
terminals tr = map tree $ filter isLeaf $ map fromTree (subTrees tr)

-- get all subtrees of a tree, where `subtree-of'
-- is taken to be a reflexive relation.
subTrees :: Eq a => Tree a -> [Tree a]
subTrees t
    | [] == (subForest t) = [t]
    | otherwise = t:foldl1 (++) (map subTrees kids)
    where
      kids = subForest t
