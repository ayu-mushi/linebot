module LineScript (lsParser) where
import Control.Monad (mplus, mzero, MonadPlus, msum)
import Text.Parsec as Parsec

-- LINE Script
-- 機能候補: 名前からメッセージを送る機能

defVarParser :: (Monad m) => ParsecT String u m (LSSentence String)
defVarParser = Parsec.try $ do
  string "var"
  many1 space
  v <- varNameParser
  return $ DefVar v

initVarParser :: (Monad m) => ParsecT String u m (LSSentence String)
initVarParser = Parsec.try $ do
  string "var"
  many1 space
  v <- varNameParser
  skipMany space
  char '='
  skipMany space
  exp <- exprParser
  return $ InitVar v exp

sentenceParser ::  (Monad m) => ParsecT String u m (LSSentence String)
sentenceParser = initVarParser <|> defVarParser <|> substParser <|> seqSentParser

seqParser ::  (Monad m) => ParsecT String u m [LSSentence String]
seqParser = Parsec.try $ do
  s <- sentenceParser
  skipMany space
  t <- (<|>) ([] <$ eof) $ do
    string ";"
    skipMany space
    t <- seqParser
    return t
  return $ s:t

substParser :: (Monad m) => ParsecT String u m (LSSentence String)
substParser = do
  v <- varNameParser
  skipMany space
  char '='
  skipMany space
  exp <- exprParser
  return $ Substitution v exp

exprParser ::  (Monad m) => ParsecT String u m (LSFormula String)
exprParser = literalParser <|> useVarParser

varNameParser :: (Monad m) => ParsecT String u m String
varNameParser = do
  init <- lower
  rest <- many $ letter <|> digit
  return $ init:rest

useVarParser :: (Monad m) => ParsecT String u m (LSFormula String)
useVarParser = Parsec.try $ do
  init <- lower
  rest <- many $ letter <|> digit -- 小文字
  return $ UseVar $ init:rest

literalParser :: (Monad m) => ParsecT String u m (LSFormula String)
literalParser = Parsec.try $ do
  (LSTrue <$ string "true")
  <|> (LSFalse <$ string "false")
  <|> (LSNumberC . read <$> (Parsec.try $ many1 digit >> char '.' >> many1 digit))
  <|> (LSNumberC . read <$> many1 digit)

lsParser ::  (Monad m) => ParsecT String u m String
lsParser = Parsec.try $ do
  msum $ map (Parsec.try . string) ["ls", "linescript", "l"]
  skipMany space
  sent <- seqParser
  return $ show sent

seqSentParser :: (Monad m) => ParsecT String u m (LSSentence String) -- 二重は?
seqSentParser = do
  char '{'
  skipMany space
  s <- seqParser
  return $ Seq $ s where

  seqParser = (<|>) ([] <$ char '}') $ Parsec.try $ do
    s <- sentenceParser
    skipMany space
    string ";"
    skipMany space
    t <- seqParser
    return $ s:t

continueParser :: (Monad m) => ParsecT String u m (LSSentence String)
continueParser = do
  string "continue"
  return Continue

data LSSentence a = If (LSFormula a) (LSSentence a) | While (LSFormula a) (LSSentence a) | Continue | DefVar a | InitVar a (LSFormula a) | Substitution a (LSFormula a) | Seq [LSSentence a] deriving (Show)
data LSFormula a = UseVar a | LSTrue | LSFalse | LSNumberC Float deriving (Show)
data LSType = LSBool | LSString | LSNumber | LSArray deriving (Show)

-- ()は式の結合の優先順位、{}は文の結合の優先順位
