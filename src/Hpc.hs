{-# LANGUAGE LambdaCase #-}

module Hpc where

import Control.Applicative (Applicative, Alternative, empty, (<|>))
import Control.Monad ((>=>))
import GHC.Base (Semigroup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

instance Monad Parser where
  pa >>= f = Parser $ \s ->
    case parse pa s of
        ParseResult (Right x) s' -> parse (f x) s'
        ParseResult (Left  e) s' -> ParseResult (Left e) s'

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
char predicate = do
  c <- anyChar
  if predicate c
    then return c
    else failure UnexpectedInput

literal :: String -> Parser String
literal = foldr op (success "")
    where
        op pc ps = (:) <$> char (== pc) <*> ps
