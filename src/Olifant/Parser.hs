{-|
Module      : Olifant.Parser
Description : First phase of the compilation
-}
--
-- It's ok to throw away results of do notation in a parser. Disable the warning
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Olifant.Parser where

import Olifant.Core hiding (lambda)

import Prelude   (Char, String, read)
import Protolude hiding (bool, many, try, (<|>))

import Data.Char   (isAlpha)
import Data.Text   (strip)
import Text.Parsec

-- ParserT monad transformer and Parser type
--
-- @ParsecT s u m ui@ is a parser with stream type s, user state type u,
-- underlying monad m and return type a. Parsec is strict in the user state.
--
-- | Special symbols
specials :: String
specials = [':', 'λ', '#', '\\', '/', ';', '\n']

-- | Term separator
sep :: Parsec Text st Char
sep = char ';' <|> newline <|> (eof *> return ';')

-- | Parse a type
ty :: Parsec Text st Ty
ty = do
    t <- optionMaybe $ try (char ':') *> (char 'i' <|> char 'b')
    return $ case t of
        Just 'b' -> TBool
        Just 'i' -> TInt
        Just _   -> TUnit
        Nothing  -> TUnit

-- | Parse a signed integer
number :: Parsec Text st Calculus
number = CLit . Number <$> ps
  where
    ps :: Parsec Text st Int
    ps = try $ do
      sign <- option ' ' (char '-')
      d <- read <$> many1 digit
      return $ if sign == '-' then negate d else d

-- | Parse scheme style boolean
--
-- Try is required on the left side of <|> to prevent eagerly consuming #
bool :: Parsec Text st Calculus
bool = CLit . Bool . (== "#t") <$> (try (string "#t") <|> string "#f")

-- | Parse an identifier
identifier :: Parsec Text st Text
identifier = toS <$> many1 (satisfy $ \c -> isAlpha c && (c `notElem` specials))

-- | Parse a word as an identifier
symbol :: Parsec Text st Calculus
symbol = CVar <$> identifier

-- [TODO] - Add support for Haskell style type declaration
-- [TODO] - Treat type declarations without body as extern

-- | Parse expressions of the form @\x.x@
lambda :: Parsec Text st Calculus
lambda = do
    choice $ map char ['\\', '/', 'λ']
    ps <- sepEndBy1 param (many1 space)
    char '.'
    body <- calculus
    return $ CLam "λ" ps body
  where
    param :: Parsec Text st (Ty, Text)
    param = do
        arg <- identifier
        t <- ty
        return (t, arg)

-- [TODO] - Drop the let for Haskell style fn definitions
bind :: Parsec Text st Calculus
bind = do
    try $ string "let"
    var <- spaces *> identifier <* spaces
    char '='
    val <- many1 space *> term <* spaces
    case val of
      CLam _name as body -> return $ CLet var (CLam var as body)
      _ -> return $ CLet var val

-- | A term, which is anything except lambda application
term :: Parsec Text st Calculus
term =
    bind <|> lambda <|> symbol <|> bool <|> number <|>
    (char '(' *> term <* char ')')

-- Calculus
calculus :: Parsec Text st Calculus
calculus = do
    f <- spaces *> term <* spaces
    as <- many (spaces *> term <* spaces)
    case as of
        [] -> return f
        _  -> return $ CApp f as

parser :: Parsec Text st [Calculus]
parser = calculus `sepEndBy1` sep <* eof

-- | Parse source and return AST
--
-- Converting ParseError to Text is losing information, but helps compose
-- elsewhere. See `Test.exec` for example. This is alright because I'm not doing
-- anything else with it right now.
parse :: Text -> Either Error [Calculus]
parse "" = Right []
parse input =
    case runP parser () "" (strip input) of
        Left err  -> Left $ ParseError err
        Right val -> Right val
