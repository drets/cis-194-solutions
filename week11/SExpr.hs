-- Description: http://www.seas.upenn.edu/~cis194/spring13/hw/11-applicative2.pdf
-- AParser.hs: http://www.seas.upenn.edu/~cis194/spring13/extras/11-applicative2/AParser.hs

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore = many


oneOrMore :: Parser a -> Parser [a]
oneOrMore = some

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b x = a *> x <* b

parseSExpr :: Parser SExpr
parseSExpr = between spaces
                     spaces
                     (A <$> parseAtom <|>
                      Comb <$> between (char '(') (char ')') (oneOrMore parseSExpr))
