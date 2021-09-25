{-# LANGUAGE LambdaCase #-}

module ApplicativeParser where

import Data.Foldable (asum)
import Data.Functor (($>))
import Control.Applicative (Applicative, Alternative, empty, (<|>), many, some)
import Data.Char

data ParseResult a =
    ParseResult (Either ParseError a) String
    deriving Show

data ParseError
    = PrematureEOF
    | ExpectedEOF
    | UnexpectedEOF
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
        (c:cs) -> ParseResult (Right c) cs
        _      -> ParseResult (Left UnexpectedEOF) ""

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP predicate parser = Parser $
    \input ->
        case parse parser input of
            ParseResult (Right result) s
                 | predicate result
                -> ParseResult (Right result) s
            _
                -> ParseResult (Left UnexpectedInput) input


char :: (Char -> Bool) -> Parser Char
char = flip filterP anyChar

literal :: String -> Parser String
literal = foldr op (success "")
    where
        op c ps = (:) <$> char (== c) <*> ps

toString :: a -> [a]
toString = (:[])

digit :: Parser String
digit = toString <$> char isDigit

integer :: Parser Integer
integer = read <$> (mconcat <$> some digit)

chars :: (Char -> Bool) -> Parser String
chars predicate = mconcat <$> many (toString <$> char predicate)

string :: Parser String
string = literal "\"" *> chars (/= '"') <* literal "\""

separated :: Parser a -> Parser b -> Parser [a]
separated parser separator =
    (:) <$> parser <*> many (separator *> parser)
    <|> success []


data JsonValue
    = JsonString String
    | JsonNumber Integer
    | JsonObject [(String, JsonValue)]
    | JsonArray [JsonValue]
    | JsonBool Bool
    | JsonNull
    deriving Show

jsonNull :: Parser JsonValue
jsonNull = literal "null" $> JsonNull

jsonBool :: Parser JsonValue
jsonBool = literal "true" $> JsonBool True
       <|> literal "false" $> JsonBool False

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> integer

jsonString :: Parser JsonValue
jsonString = JsonString <$> string

jsonArray :: Parser JsonValue
jsonArray =
    let
        values = JsonArray <$> separated jsonValue (literal ",")
    in
        literal "[" *> values <* literal "]"

jsonObject :: Parser JsonValue
jsonObject =
    let
        binding = (,) <$> (string <* literal "=") <*> jsonValue
        bindings = separated binding (literal ",")
    in
        JsonObject <$> (literal "{" *> bindings <* literal "}")

jsonValue :: Parser JsonValue
jsonValue = jsonString
        <|> jsonNumber
        <|> jsonObject
        <|> jsonArray
        <|> jsonBool
        <|> jsonNull