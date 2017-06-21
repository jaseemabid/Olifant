-----------------------------------------------------------------------------
-- |
-- Module      :  Olifant.Parser
--
-----------------------------------------------------------------------------

-- | It's ok to throw away results of do notation in a parser. Disable the warning
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Olifant.Parser where

import Olifant.Calculus

import Protolude (Text, toS)
import qualified Prelude as P
import Prelude hiding (read)

import Data.Text (strip)
import Data.Char (isAlpha)
import Text.Parsec

-- ParserT monad transformer and Parser type
--
-- @ParsecT s u m ui@ is a parser with stream type s, user state type u,
-- underlying monad m and return type a. Parsec is strict in the user state.
--

-- | Special symbols
specials :: String
specials = ['λ', '#', '\\', '/', ';', '\n']

-- | Term separator
sep :: Parsec Text st Char
sep = char ';' <|> newline <|> (eof *> return ';')

-- | Parse a signed integer
number :: Parsec Text st Calculus
number = Number <$> p
  where
    p :: Parsec Text st Int
    p = try $ do
        sign <- option ' ' (char '-')
        d <- P.read <$> many1 digit
        return $ if sign == '-' then negate d else d

-- | Parse scheme style boolean
--
-- Try is required on the left side of <|> to prevent eagerly consuming #
bool :: Parsec Text st Calculus
bool = Bool . (== "#t") <$> (try (string "#t") <|> string "#f")

-- | Parse an identifier
identifier :: Parsec Text st Text
identifier = toS <$> many1 (satisfy $ \c -> isAlpha c && (c `notElem` specials))

-- | Parse a word as an identifier
symbol :: Parsec Text st Calculus
symbol = Var <$> identifier

-- | Parse expressions of the form @\x.x@
lambda :: Parsec Text st Calculus
lambda = do
    choice $ map char ['\\', '/', 'λ']
    arg <- identifier
    char '.'
    body <- calculus
    return $ Lam arg body

bind :: Parsec Text st Calculus
bind = do
    try $ string "let"
    var <- spaces *> identifier <* spaces
    char '='
    val <- many1 space *> term <* spaces
    return $ Let var val

-- | A term, which is anything except lambda application
term :: Parsec Text st Calculus
term = bind <|> lambda <|> symbol <|> bool <|> number

-- Calculus
calculus :: Parsec Text st Calculus
calculus = do
    a <- spaces *> term <* spaces
    b <- optionMaybe $ spaces *> calculus <* spaces
    return $ maybe a (App a) b

parser :: Parsec Text st [Calculus]
parser = calculus `sepEndBy1` sep <* eof

-- | Parse source and return AST
--
-- Converting ParseError to Text is losing information, but helps compose
-- elsewhere. See `Test.exec` for example. This is alright because I'm not doing
-- anything else with it right now.
read :: Text -> Either ParseError [Calculus]
read "" = Right []
read input =
    case parse parser "" (strip input) of
        Left err -> Left err
        Right val -> Right val
