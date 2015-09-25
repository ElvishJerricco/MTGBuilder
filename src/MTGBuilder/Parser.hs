module MTGBuilder.Parser (
    deckParser,
    parseDeckString,
    parseDeckFile,
    parseDeckFileOrFail
) where

import MTGBuilder.Deck
import MTGBuilder.Options
import System.IO
import Control.Monad
import Control.Monad.Reader
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
symbol = Token.symbol deckTokens
natural = Token.natural deckTokens
brackets = Token.brackets deckTokens
whiteSpace = Token.whiteSpace deckTokens

deckParser :: Parser Deck
deckParser = do
    whiteSpace
    d <- deck
    optionMaybe sideboard
    return d

deck :: Parser Deck
deck = do
    cards <- many $ try cardParser
    return $ unions cards

cardParser :: Parser (Set Card)
cardParser = lexeme (mainboardCard <|> (sideboardCard >> return empty))

mainboardCard :: Parser (Set Card)
mainboardCard = do
    numCopies <- natural
    set <- optionMaybe $ brackets $ optionMaybe identifier
    name <- manyTill anyChar endOfCard
    return $ fromList [MkCard {name=name,copy=fromIntegral n} | n <- [1..numCopies]]
    where endOfCard = (endOfLine >> return ()) <|> eof

sideboardCard :: Parser ()
sideboardCard = do
    symbol "SB:"
    mainboardCard
    return ()

sideboard :: Parser ()
sideboard = do
    reserved "sideboard"
    deck
    return ()

parseDeckString = parse deckParser
parseDeckFile = parseFromFile deckParser

parseDeckFileOrFail :: String -> ReaderT Options IO (String, Deck)
parseDeckFileOrFail file = do
    Options {optVerbose=verbose} <- ask
    when verbose $ liftIO $ hPutStrLn stderr ("Parsing deck: " ++ file)
    result <- liftIO $ parseDeckFile file
    case result of
        Left err    -> fail $ show err
        Right deck  -> do
            return (file, deck)