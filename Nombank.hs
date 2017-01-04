module Nombank where

import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

data Nom = Nom { file :: String
               , tree :: Int
               , token :: Int
               , lemma :: String
               , sense :: Int
               , labels :: [Piece]
               } deriving (Show)

data Piece = Piece { pointers :: String
                   , label :: String
                   } deriving (Show)

--- lexer, just to make the code more compact
lexer = Token.makeTokenParser emptyDef

lexeme = Token.lexeme lexer
decimal = Token.decimal lexer
----------------------------------------------

parseString :: Parser String
parseString = do
  s <- many1 $ noneOf " \n"
  return s

pieces :: Parser Piece
pieces = do
  pointers <- many1 (digit <|> oneOf ":,*")
  char '-'
  label <- lexeme parseString
  return (Piece pointers label)
           
prop :: Parser Nom
prop = do
  file <- lexeme parseString
  tree <- read <$> lexeme (many1 digit)
  token <- read <$> lexeme (many1 digit)
  lemma <- lexeme parseString
  sense <- read <$> lexeme (many1 digit)
  labels <- many pieces
  return (Nom file tree token lemma sense labels)
