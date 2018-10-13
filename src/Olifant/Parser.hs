{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Olifant.Parser
Description : First phase of the compilation

Grammar:

    A Program is a series of top level blocks
    Blocks are separated by new lines or ;
    Blocks contains a list of indented statements optionally separated by ;

-}

-- It's ok to throw away results of do notation in a parser. Disable the warning
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Olifant.Parser where

import Olifant.Core

import Prelude   (Char, String)
import Protolude hiding (bool, handle, many, some, try, (<|>))

import Data.Char (isAlpha)

import           Control.Monad              (fail)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type alias
type Parser = Parsec Void String

-- | Pattern match a constant without repeating it.
pattern Eql :: Calculus
pattern Eql = CVar TUnit "__EQUAL__"

-- | Comments, the Haskell way
comment :: Parser ()
comment = L.skipLineComment "--"

-- | Space consumer, with newlines
scn :: Parser ()
scn = L.space space1 comment empty

-- | Space consumer, without newlines
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) comment empty
  where
    f x = x == ' ' || x == '\t'

-- | Lexeme; consume the spaces after a token, but not before it
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a constant string literal
symbol :: String -> Parser String
symbol = L.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse a signed integer
--
-- The `signed` combinator from Megaparsec accepts spaces b/w the sign and
-- number; so that is not what I want.
number :: Parser Calculus
number = CLit . Number <$> ps
  where
    ps :: Parser Int
    ps = try $ do
      sign <- optional (char '-')
      d    <- lexeme L.decimal
      return $ if sign == Just '-' then negate d else d

-- | Parse scheme style boolean
--
-- Try is required on the left side of <|> to prevent eagerly consuming #
bool :: Parser Calculus
bool = CLit . Bool . (== "#t") <$> (try (symbol "#t") <|> symbol "#f")

-- | Parse an identifier
identifier :: Parser Text
identifier = toS <$> some (satisfy ok)
  where
    ok :: Char -> Bool
    ok c = (isAlpha c || c `elem` allowed) && (c `notElem` specials)

    -- | Special symbols
    specials :: String
    specials = [':', 'λ', '#', '\\', '/', ';', '\n']

    -- | Special symbols allowed in identifiers
    allowed :: String
    allowed = ['?', '!', '_', '+', '-', '/', '*', '^', '<', '>', '$']

-- | Parse a word as an identifier
var :: Parser Calculus
var = do
    n <- identifier
    t <- try ty
    return $ CVar t n
  where
    -- | Parse a type
    ty :: Parser Ty
    ty = do
        t <- optional $ try (char ':') *> (char 'i' <|> char 'b')
        return $ case t of
          Just 'b' -> TBool
          Just 'i' -> TInt
          Just _   -> TUnit
          Nothing  -> TUnit

-- [TODO] - Add support for Haskell style type declaration
-- [TODO] - Treat type declarations without body as extern

-- | Parse an assignment; @literal = symbol@
--
-- Using magic constants kind of suck; find some other approach
equals :: Parser Calculus
equals = CVar TUnit . toS <$> symbol "=" *> return Eql

-- | A single term; the atomic unit in the grammar
term1 :: Parser Calculus
term1 = lexeme $ bool <|> number <|> var <|> equals

-- | A sequence of terms, optionally parenthesized
terms :: Parser [Calculus]
terms = parens terms <|> many term1

-- | A sequence of terms; reduced to a single term
term :: Parser Calculus
term = terms >>= handle

-- | A single expression in the language
--
calculus :: Parser Calculus
calculus = L.nonIndented sc (L.indentBlock scn fn)
  where
    fn :: Parser (L.IndentOpt Parser Calculus Calculus)
    fn = do
        -- Header is the unintended block, which could be a simple expression or
        -- function header
        header <- terms
        return $ L.IndentMany Nothing (f header) term

    -- This function was the kind of pain impossible to explain. Megaparsec is a
    -- horrible PITA to understand and use correctly, which I'll avoid at all
    -- costs from now. A stream of tokens produced by Alex or happy is the way
    -- to go. I really should have been dealing with a list of tokens
    -- *INCLUDING* newlines and indentations.
    f :: [Calculus] -> [Calculus] -> Parser Calculus
    f header body =
        case break (Eql ==) header of
            -- _ = \n
            (_, [Eql]) -> handle $ header ++ body
            -- _ = ...
            (left, Eql: right) -> do
                f' <- handle right
                handle $ left ++ [Eql, f'] ++ body
            _ -> handle $ header ++ body

-- | Parse the whole program; split by new line
parser :: Parser [Calculus]
parser = someTill calculus eof

-- | Convert a series of terms into a Calculus expression
-- [FIX] - Error is reported after the line, be more specific
handle :: [Calculus] -> Parser Calculus
handle []  = fail "Unexpected end of input!!"
handle [x] = return x
handle ts  = case break (Eql ==) ts of
    -- ` = ..` should be a parse error, but handle it here too
    ([], _)                       -> fail "Missing left hand side of = operator"

    -- Assignment to literals is silly
    ([CLit _], _)                 -> fail "Illegal assignment to literals"

    -- Literals are not functions
    (CLit _: _, _)                -> fail "Function names cannot be literals"

    -- Assignment; `a = 42` or `x = sum 1 2`
    ([CVar t variable], Eql: rhs) -> handle rhs >>= return . CLet t variable

    -- Assignment to non text value; `3 = 4`
    ([_], [Eql, _])               -> fail "Illegal Assignment"

    -- Function definition
    (CVar _ f: as, Eql: body)     -> do

        -- Ensure all arguments are typed
        args <- mapM mkArgs as
        return $ CLam f args body
      where
        mkArgs :: Calculus -> Parser (Ty, Text)
        mkArgs (CVar t val) = return (t, val)
        mkArgs _            = fail "Expected typed variable as argument"

     -- A sequence without a = should be an application
    (f:args, [])                  -> return $ CApp f args

    -- Fail for anything we don't explicitly handle
    _                             -> fail $ "Unable to parse " <> show ts

parse' :: Parser [Calculus] -> Text -> Either Error [Calculus]
parse' _ "" = Right []
parse' p' input =
    case runParser p' "" (toS input) of
        Left err  -> Left $ ParseError $ toS $ errorBundlePretty err
        Right val -> Right val

-- | Parse source and return AST
parse :: Text -> Either Error [Calculus]
parse = parse' parser

-- | Parse source and return AST with tracing output
debug :: Text -> Either Error [Calculus]
debug = parse' $ dbg "TEST" parser
