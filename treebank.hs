module NLP.Treebank3 (parseFile, queryLabel) where

import NLP.PennTreebank
import Data.Tree
import Data.Tree.Zipper
import Data.String.Utils
import Text.ParserCombinators.Parsec hiding (label)

type Label = String

-- Wrapper around the parser, calling it as many times as possible on
-- a string, and returning a list.
parseText :: String -> [Tree String]
parseText text = case parse (many parseTree) "wsj" (strip text) of
                   Left err -> error $ "Input:\n" ++ show text ++
                               "\nError:\n" ++ show err
                   Right result -> result

-- Send the content of a file to parseText
parseFile :: FilePath -> IO [Tree String]
parseFile f = do
  text <- readFile f
  return $ parseText (strip text)

-- search function to get all positions of subtrees with a certain label
queryLabel :: Label -> Tree String -> [TreePos Full String]
queryLabel l tr = filter (eqLabel l . tree) $ descendants (fromTree tr)

-- helper for the search function
eqLabel :: Label -> Tree String -> Bool
eqLabel l tr = rootLabel tr == l
               
treeToString :: Tree String -> String
treeToString tr = undefined

terminals :: Tree String -> [Tree String]
terminals tr = map tree $ filter isLeaf $ descendants (fromTree tr)

-- get all descendants of a certain position in the tree, where `descendant-of'
-- is taken to be a reflexive relation.
descendants :: TreePos Full a -> [TreePos Full a]
descendants pos = pos:foldl1 (++) (map descendants kids)
    where
      kids = map fromTree (forest (children pos))
