module Parser where

import Data.Char

-- 8.2. パーサの型
type Parser a = String -> [(a, String)]


-- 8.3. 基本的なパーサー
_return   :: a -> Parser a
_return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                [] -> []
                (x: xs) -> [(x, xs)]

parse       :: Parser a -> String -> [(a, String)]
parse p inp = p inp


-- 8.4. 連結 (bind)
--(>>=)   :: Parser a -> (a -> Parser b) -> Parser b
--p >>= f = \inp -> case parse p inp of
--                    [] -> []
--                    [(v, out)] -> parse (f v) out
(>>>=)     :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> case parse p inp of
                     [] -> []
                     [(v, out)] -> parse (f v) out

--do doesn't work yet
--p :: Parser (Char, Char)
--p = do x <- item
--       item
--       y <- item
--       return (x, y)

p :: Parser (Char, Char)
p = item >>>= \x ->
    item >>>= \_ ->
    item >>>= \y ->
    _return (x, y)


-- 8.5. 選択
(+++)   :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    [] -> parse q inp
                    [(v, out)] -> [(v, out)]


-- 8.6. パーサーの部品
---- 8.6.1. 文字を判定

--sat :: (Char -> Bool) -> Parser Char
--sat p = do x <- item
--           if p x then return x else failure
sat :: (Char -> Bool) -> Parser Char
sat p = item >>>= \x ->
        if p x then _return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlpha

char :: Char -> Parser Char
char x = sat (== x)

---- 8.6.2. 文字列を判定
string    :: String -> Parser String
--string [] = return []
--string (x: xs) = do char x
--                    string xs
--                    return (x: xs)
--string (x: xs) = do x'  <- char x
--                    xs' <- string xs
--                    return (x':xs')
string [] = _return []
string (x: xs) = char x    >>>= \x'  ->
                 string xs >>>= \xs' ->
                 _return (x: xs)

---- 8.6.3. 同じ判定を繰り返す
many    :: Parser a -> Parser [a]
many p  = many1 p +++ _return []
many1   :: Parser a -> Parser [a]
--many1 p = do v <- p
--             vs <- many p
--             return (v: vs)
many1 p = p      >>>= \v ->
          many p >>>= \vs ->
          _return (v: vs)


---- 8.6.4. 識別子（変数名）を判定
ident :: Parser String
--ident = do x <- lower
--           xs <- many alphanum
--           return (x: xs)
ident = lower >>>= \x ->
        many alphanum >>>= \xs ->
        _return (x: xs)

nat :: Parser Int
--nat = do xs <- many1 digit
--         return (read xs)
nat = many1 digit >>>= \xs ->
      _return (read xs)

space :: Parser ()
--space = do many (sat isSpace)
--           return ()
space = many (sat isSpace) >>>= \x ->
        _return ()


-- 8.7. 空白の扱い

token   :: Parser a -> Parser a
--token p = do space
--             v <- p
--             space
--             return v
token p = space >>>= \s1 ->
          p >>>=     \v  ->
          space >>>= \s2 ->
          _return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol    :: String -> Parser String
symbol xs = token (string xs)

p' :: Parser [Int]
--p = do symbol "["
--       n  <- natural
--       ns <- many (do symbol ","
--                      natural)
--       symbol "]"
--       return (n: ns)
p' = symbol "["     >>>= \s1 ->
     natural        >>>= \n  ->
     many (
         symbol "," >>>= \s2 ->
         natural
     )              >>>= \ns ->
     symbol "]"     >>>= \s3 ->
     _return (n: ns)

-- 8.8. 数式

{-
-- version 1
expr ::= expr '+' expr | expr '*' expr | '(' expr ')' | nat
nat  ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'| '9'

-- 問題点：加法よりも乗法のほうが結合順序が高いことを表現できていない
-- version 2
expr   ::= expr '+' expr | term
term   ::= term '*' term | factor
factor ::= '(' expr ')' | nat
nat    ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'| '9'

-- 問題点：右結合であることを表現できていない
-- version 3
expr :== term '+' expr | term
term :== factor '*' term | factor
factor ::= '(' expr ')' | nat
nat    ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'| '9'

-- さらに空文字列をサポートして改善
-- version 4
expr :== term ('+' expr | epsilon)
term :== factor ('*' term | epsilon)
factor ::= '(' expr ')' | nat
nat    ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'| '9'
-}

expr :: Parser Int
--expr = do t <- term
--          do symbol "+"
--             e <- expr
--             return (t + e)
--          +++ return t
expr = term                     >>>= \t ->
           (
               symbol "+"       >>>= \s ->
               expr             >>>= \e ->
               _return (t + e)
           )
           +++ _return t

term :: Parser Int
--term = do f <- factor
--          do symbol "*"
--             t <- term
--             return (f * t)
--          +++ return f
term = factor                   >>>= \f ->
           (
               symbol "*"       >>>= \s ->
               term             >>>= \t ->
               _return (f * t)
           )
           +++ _return f

factor :: Parser Int
--factor = do symbol "("
--            e <- expr
--            symbol ")"
--            return e
--         +++ natural
factor = (
             symbol "("  >>>= \s1 ->
             expr        >>>= \e  ->
             symbol ")"  >>>= \s2 ->
             _return e
         )
         +++ natural

--eval    :: String -> Int
--eval xs = case parse expr xs of
--            [(n, [])]  ->  n
--            [(_, out)] -> error("unused input" ++ out)
--            []         -> error "invalid input"

