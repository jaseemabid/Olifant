{-# LANGUAGE PatternSynonyms #-}
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
import Protolude hiding (bool, handle, many, try, (<|>))

import Data.Char   (isAlpha)
import Data.Text   (strip)
import Text.Parsec hiding (space, spaces)

-- ParserT monad transformer and Parser type
--
-- @ParsecT s u m ui@ is a parser with stream type s, user state type u,
-- underlying monad m and return type a. Parsec is strict in the user state.

-- | Handle a single space. Parsec version consumes new line as well
space :: Parsec Text st Char
space = char ' '

-- | Skips /zero/ or more white space characters.
--
-- Redefining spaces because we redefined `space` as well
spaces :: Parsec Text st ()
spaces = skipMany space <?> "white space"

-- | Pattern match a constant without repeating it.
pattern Eql :: Calculus
pattern Eql = CVar TUnit "__EQUAL__"

-- | Term separator
sep :: Parsec Text st Char
sep = char ';' <|> newline <|> (eof *> return ';')

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
identifier = toS <$> many1 (satisfy ok)
  where
    ok :: Char -> Bool
    ok c = (isAlpha c || c `elem` allowed) && (c `notElem` specials)

    -- | Special symbols
    specials :: String
    specials = [':', 'λ', '#', '\\', '/', ';', '\n']

    -- | Special symbols allowed in identifiers
    allowed :: String
    allowed = ['?', '_']

-- | Parse a word as an identifier
symbol :: Parsec Text st Calculus
symbol = do
    n <- identifier
    t <- try ty
    return $ CVar t n
  where
    -- | Parse a type
    ty :: Parsec Text st Ty
    ty = do
        t <- optionMaybe $ try (char ':') *> (char 'i' <|> char 'b')
        return $ case t of
          Just 'b' -> TBool
          Just 'i' -> TInt
          Just _   -> TUnit
          Nothing  -> TUnit

-- [TODO] - Add support for Haskell style type declaration
-- [TODO] - Treat type declarations without body as extern

-- | Parse a literal = symbol
--
-- Using magic constants kind of suck; find some other approach
equals :: Parsec Text st Calculus
equals = CVar TUnit . toS <$> string "=" *> return Eql

-- | A term, which is anything except lambda application
term :: Parsec Text st Calculus
term = symbol <|> bool <|> number <|> equals

-- | Parse a single calculus expression
calculus :: Parsec Text st Calculus
calculus = manyTill (spaces *> term <* spaces) (try sep) >>= handle

-- | Parse the whole program; split by new line
parser :: Parsec Text st [Calculus]
parser = sepEndBy1 (spaces *> calculus <* spaces) spaces <* eof

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

-- | Handle a series of terms into a Calculus expression
handle :: [Calculus] -> Parsec Text st Calculus
handle []  = parserFail "Oops!"
handle [x] = return x
handle ts  = case break (Eql ==) ts of
    -- Assignment; `a = 42`
    ([CVar t var], [Eql, val]) -> return $ CLet t var val

    -- Assignment to non text value; `3 = 4`
    ([_], [Eql, _])          -> parserFail "Illegal Assignment"

    -- Function definition
    (CVar _ f: as, Eql: body) -> do

        -- Ensure all arguments are typed
        args <- mapM mkArgs as
        body' <- handle body
        return $ CLam f args body'
      where
        mkArgs :: Calculus -> Parsec Text st (Ty, Text)
        mkArgs (CVar t val) = return (t, val)
        mkArgs _ = parserFail "Expected typed variable as argument"

     -- A sequence without a = should be an application
    (f:args, []) -> return $ CApp f args

    _ -> parserFail $ "Unable to parse\n" <> show ts
