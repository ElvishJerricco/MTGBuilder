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
import Data.Set (Set)
import qualified Data.Set as Set
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
    Set.union <$> deck <*> option Set.empty sideboard

deck :: Parser Deck
deck = Set.unions <$> (many $ try cardParser)

cardParser :: Parser (Set Card)
cardParser = lexeme (mainboardCard <|> sideboardCard)

mainboardCard :: Parser (Set Card)
mainboardCard = do
    numCopies <- natural
    set <- optionMaybe $ brackets $ optionMaybe identifier
    name <- manyTill anyChar endOfCard
    return $ Set.fromList [MkCard {name=name,copy=fromIntegral n,isSideboard=False} | n <- [1..numCopies]]
    where endOfCard = (endOfLine >> return ()) <|> eof

setSideboard :: Card -> Card
setSideboard card = card {isSideboard = True}

sideboardCard :: Parser (Set Card)
sideboardCard = do
    symbol "SB:"
    Set.map setSideboard <$> mainboardCard

sideboard :: Parser Deck
sideboard = do
    reserved "sideboard"
    Set.map setSideboard <$> deck

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