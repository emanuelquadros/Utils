import NLP.PennTreebank
import Data.Tree
import Data.String.Utils
import Text.ParserCombinators.Parsec
import System.Environment
import Data.List

-- getTree 
    
main = do
  input <- getArgs
  wsj <- (readFile (head input))
  let tree = case parse parseTree "wsj" (strip wsj) of
               Left err -> error $ "Input:\n" ++ show wsj ++
                           "\nError:\n" ++ show err
               Right result -> result
  putStrLn "DEBUG: parsed:"; putStrLn (drawTree tree)
