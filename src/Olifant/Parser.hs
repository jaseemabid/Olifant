-----------------------------------------------------------------------------
-- |
-- Module      :  Olifant.Parser
--
-----------------------------------------------------------------------------
module Olifant.Parser where

import Data.Text (strip)
import Olifant.Calculus
import Prelude hiding (read)
import Protolude (Text, toS)
import Text.Parsec
import qualified Prelude as P

-- ParserT monad transformer and Parser type

-- @ParsecT s u m ui@ is a parser with stream type s, user state type u,
-- underlying monad m and return type a. Parsec is strict in the user state.
--

-- | Parse a signed integer
number :: Parsec Text st Calculus
number = Number <$> p
  where
    p :: Parsec Text st Int
    p = try $ do
        sign <- option ' ' (char '-')
        d <- P.read <$> many1 digit
        return $ if sign == '-' then negate d else d

-- | Parse a word as an identifier
symbol :: Parsec Text st Calculus
symbol = Var . toS <$> many1 letter

-- | Parse scheme style boolean
--
-- Try is required on the left side of <|> to prevent eagerly consuming #
bool :: Parsec Text st Calculus
bool = Bool . (== "#t") <$> (try (string "#t") <|> string "#f")

-- | Parse expressions of the form @\x.1@
--
lambda :: Parsec Text st Calculus
lambda = do
    _ <- choice $ map char ['\\', '/', '^', '|']
    arg <-  toS <$> many1 letter
    _ <- char '.'
    body <- calculus
    return $ Lam arg body

application :: Parsec Text st Calculus
application = try $ do
    a <- calculus
    _ <- many1 space
    b <- calculus
    return $ App a b

-- | The lambda calculus grammar
calculus :: Parsec Text st Calculus
calculus =
      bool
  <|> number
  <|> symbol
  <|> lambda
  <|> application

parser :: Parsec Text st [Calculus]
parser = calculus `sepBy` newline

-- | Parse source and return AST
--
-- Converting ParseError to Text is losing information, but helps compose
-- elsewhere. See `Test.exec` for example. This is alright because I'm not doing
-- anything else with it right now.
read :: Text -> Either Text [Calculus]
read "" = Right []
read input =
    case parse parser "" (strip input) of
        Left err -> Left $ toS $ P.show err
        Right val -> Right val
