import NLP.PennTreebank
--import Control.Lens
import Data.Tree
import Data.List
--import Data.Tree.Lens hiding (children)
import Data.Tree.Zipper
import Data.String.Utils
import Text.ParserCombinators.Parsec hiding (label)
import Text.Parsec.String
import System.Environment
import Data.List


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

-- Search function
--search :: Label -> Tree String -> [TreePos Full a]
--search l tr = go . fromTree where
--    go loc = filter (eqLabel l . tree) children
--             (foldl1 (++) loc:(map go (childrenAsList loc)))

eqLabel :: Label -> Tree String -> Bool
eqLabel l tr = rootLabel tr == l

-- get all descendants of a certain position in the tree, where `descendant-of'
-- is taken to be a reflexive relation.
descendants :: TreePos Full a -> [TreePos Full a]
descendants pos
    | hasChildren pos = pos:foldl1 (++) (map descendants kids)
    | otherwise = []
    where
      kids = map fromTree (forest (children pos))


main = do
  input <- getArgs
  wsj <- (readFile (head input))
  putStrLn "DEBUG: parsed:"
