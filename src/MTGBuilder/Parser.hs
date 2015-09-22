module MTGBuilder.Parser (
    deckParser,
    parseDeckString,
    parseDeckFile
) where

import MTGBuilder.Deck
import Control.Monad
import Data.Set
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

deckTokens = Token.makeTokenParser $ emptyDef {
    identStart = alphaNum <|> char '_',
    commentLine = "//",
    caseSensitive = False
}

identifier = Token.identifier deckTokens
lexeme = Token.lexeme deckTokens
reserved = Token.reserved deckTokens
natural = Token.natural deckTokens
brackets = Token.brackets deckTokens
whiteSpace = Token.whiteSpace deckTokens

deckParser :: Parser Deck
deckParser = do
    whiteSpace
    d <- deck
    sideboard
    return d

deck :: Parser Deck
deck = do
    cards <- many cardParser
    return $ unions cards

cardParser :: Parser (Set Card)
cardParser = mainboardCard <|> (sideboardCard >> return empty)

mainboardCard :: Parser (Set Card)
mainboardCard = lexeme $ do
    numCopies <- natural
    set <- optionMaybe $ brackets $ optionMaybe identifier
    name <- manyTill anyChar endOfCard
    return $ fromList [MkCard {name=name,copy=fromIntegral n} | n <- [1..numCopies]]
    where endOfCard = (endOfLine >> return ()) <|> eof

sideboardCard :: Parser ()
sideboardCard = do
    reserved "SB:"
    mainboardCard
    return ()

sideboard :: Parser ()
sideboard = do
    reserved "sideboard"
    deck
    return ()

parseDeckString = parse deckParser
parseDeckFile = parseFromFile deckParser