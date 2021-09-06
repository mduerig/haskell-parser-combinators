{-# LANGUAGE LambdaCase #-}

module MonadicParser where

import ApplicativeParser
    ( JsonValue(..),
      Parser(..),
      ParseResult(ParseResult),
      success,
      char,
      toString,
      digit,
      chars )
import Data.Functor (($>))
import Data.Char (isDigit)
import Control.Monad(foldM)


instance Monad Parser where
  p >>= f = Parser $ \s ->
    case parse p s of
        ParseResult (Right a) s' -> parse (f a) s'
        ParseResult (Left  e) s' -> ParseResult (Left e) s'

andThen :: Parser a -> Parser b -> Parser (a, b)
andThen pa pb = do
    a <- pa
    b <- pb
    return (a, b)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = Parser $ \s ->
    case parse p1 s of
        ParseResult (Left _) _ -> parse p2 s
        r -> r

someM :: Parser a -> Parser [a]
someM p = do
    a  <- p
    as <- manyM p
    return (a:as)

manyM :: Parser a -> Parser [a]
manyM p = someM p `orElse` success []

literalM :: String -> Parser String
literalM = foldM op ""
    where
        op cs c = do
            c' <- char (== c)
            return (cs ++ [c])

integerM :: Parser Integer
integerM = read . concat <$> someM digit

charsM :: (Char -> Bool) -> Parser String
charsM predicate = mconcat <$> manyM (toString <$> char predicate)

stringM :: Parser String
stringM = do
    _  <- literalM "\""
    cs <- chars (/= '"')
    _  <- literalM "\""
    return cs

separatedM :: Parser a -> Parser b -> Parser [a]
separatedM parser separator =
    let
        list = do
          a  <- parser
          as <- manyM (separator >> parser)
          return (a:as)
    in
        list `orElse` pure []


jsonNullM :: Parser JsonValue
jsonNullM = literalM "null" $> JsonNull

jsonBoolM :: Parser JsonValue
jsonBoolM = (literalM "true" $> JsonBool True)
            `orElse`
            (literalM "false" $> JsonBool False)

jsonNumberM :: Parser JsonValue
jsonNumberM = JsonNumber <$> integerM

jsonStringM :: Parser JsonValue
jsonStringM = JsonString <$> stringM

jsonArrayM :: Parser JsonValue
jsonArrayM = do
    _      <- literalM "["
    values <-  separatedM jsonValueM (literalM ",")
    _      <- literalM "]"
    return $ JsonArray values

jsonObjectM :: Parser JsonValue
jsonObjectM = do
        _        <- literalM "{"
        bindings <- separatedM binding (literalM ",")
        _        <- literalM "}"
        return $ JsonObject bindings
    where
        binding = do
            key   <- stringM
            _     <- literalM "="
            value <- jsonValueM
            return (key, value)

jsonValueM :: Parser JsonValue
jsonValueM = jsonStringM
    `orElse` jsonNumberM
    `orElse` jsonObjectM
    `orElse` jsonArrayM
    `orElse` jsonBoolM
    `orElse` jsonNullM

