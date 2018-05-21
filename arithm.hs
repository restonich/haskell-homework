module Arithm where

data Term = Const Int
          | Plus Term Term
          | Mul  Term Term

eval :: Term -> Int
eval t = helper t id
  where
    helper (Const n) k = k n
    helper (Plus a b) k = helper a (\r1 -> helper b (\r2 -> k $ r1+r2))
    helper (Mul  a b) k = helper a (\r1 -> helper b (\r2 -> k $ r1*r2))

-- parsing stuff
stringConst :: String -> Parsec String u ()
stringConst [] = return ()
stringConst (c:cs) = do
  char c
  stringConst cs

decimal ops = do
  digits <- many1 digit
  pos <- getPosition
  let n = foldl (\acc d -> 10*acc + digitToInt d) 0 digits
  seq n (return n)

parser =  buildExpressionParser table (spaces *> decimal <* spaces)
          <?> "aithmetic expression"

table =
  [ [binary "*" (binop ops Mul) AssocLeft ]
  , [binary "+" (binop ops Add) AssocLeft ]
  ]

binary name fun assoc = Infix (do{ stringConst name; return fun }) assoc
