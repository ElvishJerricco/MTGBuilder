{-# LANGUAGE BangPatterns #-}

module MTGBuilder.Deck (
    makeRanking,
    composeAdditive,
    composeDecks,
    dumpDeck,
    dumpRanking,
    Ranking,
    Card(..),
    Deck
) where

import MTGBuilder.Combination
import MTGBuilder.Options
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Tuple
import Control.Monad.Reader
import System.IO

data Card = MkCard {
    name :: String,
    copy :: Int
} deriving (Show, Eq, Ord)

type Deck = Set Card

{-
The ranking is the most important data structure

http://www.channelfireball.com/articles/magic-math-a-new-way-to-determine-an-aggregate-deck-list-rg-dragons/

Frank Karsten's algorithm is an example of building a deck based soley on first order rankings.

An Nth order ranking is the popularty of a particular combination of N cards.
If six of the input decks run Bolt1+Snapcaster1, that combination gets a score of 6.

The interaction field is the map of all combinations found in any input deck,
and the number of decks that have that combination.

The interactionSize field is the maximum order of rank.
This is used to throttle the computational expense, at the cost of precision.
Precision beyond 3rd order likely isn't necessary.
2nd order is enough for most cases.
1st order is exactly Karsten's algorithm, and is only good enough for single-archetype aggregations.
-}

data Ranking = MkRanking {
    interaction :: Map (Set Card) Int,
    interactionSize :: Int
}

-- The return type of this function is a reader over IO so that verbosity can be read, and verbose messages can be printed
makeRanking :: Int -> [(String, Deck)] -> ReaderT Options IO Ranking
makeRanking size inputDecks = do
    Options {optVerbose=verbose} <- ask
    rankDecks MkRanking { interaction=Map.empty, interactionSize=size } inputDecks
    where
        rankDecks :: Ranking -> [(String, Deck)] -> ReaderT Options IO Ranking
        rankDecks ranking [] = return ranking
        rankDecks ranking ((name, deck):decks) = do
            Options {optVerbose=verbose} <- ask
            when verbose (liftIO $ hPutStrLn stderr $ "Ranking " ++ name)
            rankDecks (ranking { interaction=int }) decks
            where
                !int = -- Strict, because it will be fully evaluated anyway, and this provides more realistic verbose messages.
                    let f map x = Map.insertWith (+) x 1 map
                    in  foldl f (interaction ranking) $ Set.unions [combinations n deck | n <- [1..(interactionSize ranking)]]

dumpDeck :: Deck -> String
dumpDeck deck = intercalate "\n" lines
    where
        lines :: [String]
        lines = fmap (\(cardName, count) -> (show count) ++ " " ++ cardName) (Map.toList getMap)
            where
                getMap :: Map String Int
                getMap = foldl f Map.empty deck
                    where
                        f = (\map card -> Map.insertWith (+) (name card) 1 map)

dumpRanking :: Ranking -> String
dumpRanking ranking = intercalate "\n" lines
    where
        lines :: [String]
        lines = fmap (\(combo, count) -> (show count) ++ " : " ++ (show combo)) $ Map.toList $ interaction ranking

{-
Composition combines all the input decks.
Note: Although different copies of the same card are treated as different cards in this algorithm,
the same copies of the same card from different decks are treated as the same.
So when we union the decks, we are merely merging all the cards in all decks.
So if two decks each have 4 Bolts, we still only see 4 Bolts in the union.

Composing decks simply sorts the cards in the union by sortWithRanking,
then removes the lowest ranked card, then repeats until the deck is down to the provided size.
-}
composeDecks :: Ranking -> Int -> [Deck] -> ReaderT Options IO Deck
composeDecks ranking deckSize decks = compose $ Set.unions decks
    where
        compose :: Deck -> ReaderT Options IO Deck
        compose cards
            | Set.size sorted <= deckSize = return $ Set.map snd sorted
            | otherwise = do
                Options {optVerbose=verbose} <- ask
                when verbose $ liftIO $ hPutStrLn stderr (show $ Set.size sorted)
                compose $ Set.map snd $ fromMaybe Set.empty $ fmap snd $ Set.minView sorted
            where sorted = sortWithRanking ranking cards

-- Each card in the set is ranked.
sortWithRanking :: Ranking -> Set Card -> Set (Double, Card)
sortWithRanking ranking deck = Map.foldlWithKey (\set card rank -> Set.insert (rank, card) set) Set.empty rankMap
    where
        rankMap = Map.foldlWithKey rankCombo Map.empty $ interaction ranking
        rankCombo map combo count
            | combo `Set.isSubsetOf` deck = foldl (\m card -> Map.insertWith (+) card rank m) map combo
            | otherwise = map
            where
                {-
                To rank a card, look at each combination in the ranking.
                For each combination that contains the card, and is a subset of the deck,
                add to the card's ranking the following:

                    (popularity of combo) * 1 / (2 ^ order of combo)

                This way, lower orders are considered more important,
                thus the popularity of the card on its own (first order combination) is most important
                -}
                rank = (fromIntegral count) * 1.0 / (2.0 ^ Set.size combo)

{-
Addititive composition is similar to subtractive composition.
This new algorithm will be the new default, due to it's performance gains and added capabilities.

Rather than starting with the collective and working down,
start with nothing (or something) and work up.
That is, find the card that adds the most to the deck, and add that to it.

An advantage of this algorithm is that you can provide a starting state,
which allows you to specify cards you want the deck to be built around.
-}
composeAdditive :: Ranking -> Int -> Deck -> ReaderT Options IO Deck
composeAdditive ranking finalSize deck
    | Set.size deck >= finalSize = return deck
    | otherwise = do
        Options {optVerbose=verbose} <- ask
        when verbose $ liftIO $ hPutStrLn stderr $ show $ Set.size deck
        composeAdditive ranking finalSize (bestCard `Set.insert` deck)
    where
        (_, bestCard) = head $ sortBy (flip compare) $ fmap swap $ Map.toList rankMap
        rankMap = Map.foldlWithKey rankCombo Map.empty $ interaction ranking
        rankCombo map combo count
            | Set.size dif == 1 = Map.insertWith (+) (Set.elemAt 0 dif) rank map
            | otherwise = map
            where
                dif = combo `Set.difference` deck
                rank = (fromIntegral count) * 1.0 / (2.0 ^ Set.size combo)