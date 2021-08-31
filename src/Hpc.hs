{-# LANGUAGE LambdaCase #-}

module Hpc where

import Control.Applicative (Applicative, Alternative, empty, (<|>))
import Control.Monad ((>=>))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ParseError
    = PrematureEOF
    | ExpectedEOF
    | UnexpectedInput
    deriving Show

newtype Parser a = Parser
    { parse :: String -> Either ParseError (String, a)
    }

instance Functor Parser where
    fmap f p = Parser $ (fmap . fmap) f . parse p

instance Applicative Parser where
  pure = success
  pf <*> pa = Parser $ \s ->
      case parse pf s of
          Right (s', f) -> parse (f <$> pa) s'
          Left e -> Left e

instance Monad Parser where
  pa >>= f = Parser $ \s ->
      case parse pa s of
          Right (s', a) -> parse (f a) s'
          Left e -> Left e

instance Alternative Parser where
  empty = failure UnexpectedInput
  p1 <|> p2 = Parser $ parse p1 <> parse p2

success :: a -> Parser a
success x = Parser $ \s -> Right (s, x)

failure :: ParseError -> Parser a
failure e = Parser $ \s -> Left e

eof :: Parser ()
eof = Parser $
    \case
        "" -> Right ("", ())
        _  -> Left ExpectedEOF

anyChar :: Parser Char
anyChar = Parser $
    \case
        c:cs -> Right (cs, c)
        _    -> Left PrematureEOF

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
