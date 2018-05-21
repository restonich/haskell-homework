module ULC where
-- changed after adoption  from
-- https://github.com/wetmore/TAPL-implementations/blob/master/untyped/Parser.hs

import Prelude hiding (abs)
import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token (reservedOp)
import Text.Parsec.Combinator (between, sepBy1, chainr1)
import Data.List (elemIndex)

data Info = Info { row :: Int, col :: Int } deriving (Show)
infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

type BoundContext = [String]
type LCParser rez = Parsec String BoundContext rez

parseVarName :: Parsec String u String
parseVarName = many1 $ letter -- <|> char '\''

stringConst :: String -> Parsec String u ()
stringConst [] = return ()
stringConst (c:cs) = do
  char c
  stringConst cs

data BinOpSort = Mul | Add deriving Show
data Ops a = TermConstruction
  { app   :: Info -> a -> a -> a
  , abs   :: Info -> String -> a  -> a
  , int   :: Info -> Int -> a
  , var   :: Info -> String -> a
  , reset :: Info -> a -> a
  , call  :: Info -> String -> a -> a   -- a.k.a. shift
  , binop :: BinOpSort -> a -> a -> a
  }

parens :: Parsec String u a -> Parsec String u a
parens = between (char '(') (char ')')

decimal :: Ops a -> LCParser a
decimal ops = do
  digits <- many1 digit
  pos <- getPosition
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return $ int ops (infoFrom pos) n)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped λ-calculus"

-- parse :: Ops a -> String -> Either ParseError a
-- parse ops = parseWith $ parseTerm ops
-- --------------------------------------
parse ops = parseWith $ root ops


root ops =  buildExpressionParser (table ops) (spaces *> (apps ops) <* spaces)
            <?> "expression"
  where
    apps ops = chainl1 (parseNonApp ops) $ do
      spaces
      pos <- getPosition
      return $ app ops (infoFrom pos)

    parseNonApp ops =
                parens (spaces *> (root ops) <* spaces)  -- (M)
            <|> parseReset ops     -- reset T
            <|> parseCallCC ops    -- call k T or shift k T
            <|> parseAbs ops       -- λx.M
            <|> parseVar ops       -- x
            <|> (spaces *> (decimal ops) <* spaces)   -- 5
    parseAbs ops = do
      spaces
      char '\\' <|> char 'λ'
      v <- parseVarName
      modifyState (v :)
      spaces
      char '.'
      spaces
      t <- root ops
      modifyState tail
      pos <- getPosition
      return $ abs ops (infoFrom pos) v t

    parseCallCC ops = do
      stringConst "shift"
      spaces
      v <- parseVarName
      modifyState (v :)
      spaces
      t <- root ops
      modifyState tail
      pos <- getPosition
      return $ call ops (infoFrom pos) v t

    parseReset ops = do
      spaces
      stringConst "reset"
      spaces
      t <- root ops
      pos <- getPosition
      return $ reset ops (infoFrom pos) t

    parseVar :: Ops a -> LCParser a
    parseVar ops = do
      spaces
      v <- parseVarName
      spaces
      list <- getState
      findVar v list
      where
        --findVar :: String -> BoundContext -> LCParser a
        findVar v list = case elemIndex v list of
          Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
          Just n  -> do
            pos <- getPosition
            return $ var ops (infoFrom pos) v

table ops =
  [ [binary "*" (binop ops Mul) AssocLeft ]
  , [binary "+" (binop ops Add) AssocLeft ]
  ]

binary name fun assoc = Infix (do{ spaces *> (stringConst name); return fun }) assoc
