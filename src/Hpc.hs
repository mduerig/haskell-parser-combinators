{-# LANGUAGE LambdaCase #-}

module Hpc where

import Control.Applicative (Applicative, Alternative, empty, (<|>))
import Control.Monad ((>=>))
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

anyChar :: Parser Char
anyChar = Parser $
    \case
        c:cs -> ParseResult (Right c) cs
        ""   -> ParseResult (Left PrematureEOF) ""

char :: (Char -> Bool) -> Parser Char
char predicate = Parser $
    \case
        (c:cs) | predicate c -> ParseResult (Right c) cs
        cs                   -> ParseResult (Left UnexpectedInput) cs

literal :: String -> Parser String
literal = foldr op (success "")
    where
        op c ps = (:) <$> char (== c) <*> ps

eps :: Monoid a => Parser a
eps = success mempty

optional :: Monoid a => Parser a -> Parser a
optional p = p <|> eps

cat :: Monoid a => [Parser a] -> Parser a
cat = foldr op eps
    where
        op p q = (<>) <$> p <*> q

many :: Monoid a => Parser a -> Parser a
many p = cat [p, many p] <|> eps

anyOf :: Parser a -> [Parser a] -> Parser a
anyOf = foldr (<|>)

toString :: a -> [a]
toString = (:[])

digit :: Parser String
digit = anyOf (literal "0") (literal . toString <$> ['1'..'9'])

integer :: Parser Integer
integer = read <$> cat [digit, many digit]

chars :: (Char -> Bool) -> Parser String
chars predicate = many (toString <$> char predicate)

string :: Parser String
string = cat [literal "\"", chars (/= '"'), literal "\""]