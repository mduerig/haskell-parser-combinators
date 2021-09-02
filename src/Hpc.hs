{-# LANGUAGE LambdaCase #-}

module Hpc where

import Data.Foldable (asum)
import Control.Applicative (Applicative, Alternative, empty, (<|>), many, some)
import GHC.Base (Semigroup)

data ParseResult a =
    ParseResult (Either ParseError a) String
    deriving Show

data ParseError
    = PrematureEOF
    | ExpectedEOF
    | UnexpectedInput
    deriving Show

instance Functor ParseResult where
    fmap f (ParseResult r s) = ParseResult (fmap f r) s

instance Semigroup (ParseResult a) where
  (ParseResult (Left _) _) <> r = r
  r <> _ = r

newtype Parser a = Parser
    { parse :: String -> ParseResult a
    }

instance Functor Parser where
    fmap f p = Parser $ fmap f . parse p

instance Applicative Parser where
  pure = success
  pf <*> pa = Parser $ \s ->
    let
        ParseResult rf s'  = parse pf s
        ParseResult ra s'' = parse pa s'
    in
        ParseResult (rf <*> ra) s''

instance Alternative Parser where
  empty = failure UnexpectedInput
  p1 <|> p2 = Parser $ parse p1 <> parse p2

success :: a -> Parser a
success x = Parser $ \s -> ParseResult (Right x) s

failure :: ParseError -> Parser a
failure e = Parser $ \s -> ParseResult (Left e) s

eof :: Parser ()
eof = Parser $
    \case
        "" -> ParseResult (Right ()) ""
        _  -> ParseResult (Left ExpectedEOF) ""

char :: (Char -> Bool) -> Parser Char
char predicate = Parser $
    \case
        (c:cs) | predicate c -> ParseResult (Right c) cs
        cs                   -> ParseResult (Left UnexpectedInput) cs

anyChar :: Parser Char
anyChar = char (const True)

literal :: String -> Parser String
literal = foldr op (success "")
    where
        op c ps = (:) <$> char (== c) <*> ps

toString :: a -> [a]
toString = (:[])

digit :: Parser String
digit = asum (literal . toString <$> ['0'..'9'])

integer :: Parser Integer
integer = read <$> (mconcat <$> some digit)

chars :: (Char -> Bool) -> Parser String
chars predicate = mconcat <$> many (toString <$> char predicate)

string :: Parser String
string = literal "\"" *> chars (/= '"') <* literal "\""

separated :: Parser a -> Parser b -> Parser [a]
separated parser separator =
    (:) <$> parser <*> many (separator *> parser)
