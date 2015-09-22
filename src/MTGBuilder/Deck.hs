{-# LANGUAGE BangPatterns #-}

module MTGBuilder.Deck (
    makeRanking,
    composeDecks,
    Ranking,
    Card(..),
    Deck
) where

import MTGBuilder.Combination
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
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
makeRanking :: Int -> [(String, Deck)] -> ReaderT Bool IO Ranking
makeRanking size = rankDecks MkRanking { interaction=Map.empty, interactionSize=size }
    where
        rankDecks :: Ranking -> [(String, Deck)] -> ReaderT Bool IO Ranking
        rankDecks ranking [] = return ranking
        rankDecks ranking ((name, deck):decks) = do
            verbose <- ask
            when verbose (liftIO $ hPutStrLn stderr $ "Ranking " ++ name)
            ranked <- rankDecks (ranking { interaction=int }) decks
            return ranked
            where
                !int = -- Strict, because it will be fully evaluated anyway, and this provides more realistic verbose messages.
                    let f map x = Map.insertWith (+) x 1 map
                    in Map.unionsWith (+) [foldl f (interaction ranking) $ combinations n deck | n <- [1..(interactionSize ranking)]]

{-
Composition combines all the input decks.
Note: Although different copies of the same card are treated as different cards in this algorithm,
the same copies of the same card from different decks are treated as the same.
So when we union the decks, we are merely merging all the cards in all decks.
So if two decks each have 4 Bolts, we still only see 4 Bolts in the union.

Composing decks simply sorts the cards in the union by sortWithRanking,
then removes the lowest ranked card, then repeats until the deck is down to the provided size.
-}
composeDecks :: Ranking -> Int -> [Deck] -> ReaderT Bool IO Deck
composeDecks ranking deckSize decks = compose $ Set.unions decks
    where
        compose :: Deck -> ReaderT Bool IO Deck
        compose cards
            | Set.size sorted <= deckSize = return $ Set.map snd sorted
            | otherwise = do
                verbose <- ask
                when verbose (liftIO $ hPutStrLn stderr (show $ Set.size sorted))
                compose $ Set.map snd $ fromMaybe Set.empty $ fmap snd $ Set.minView sorted
            where sorted = sortWithRanking ranking cards

-- Each card in the set is ranked.
sortWithRanking :: Ranking -> Set Card -> Set (Double, Card)
sortWithRanking ranking deck = Set.map rank deck
    where
        {-
        To rank a card, look at each combination in the ranking.
        For each combination that contains the card, and is a subset of the deck,
        add to the card's ranking the following:

            (popularity of combo) * 1 / (2 ^ order of combo)

        This way, lower orders are considered more important,
        thus the popularity of the card on its own (first order combination) is most important
        -}
        rank :: Card -> (Double, Card)
        rank card = (Map.foldlWithKey f 0 $ interaction ranking, card)
            where
                f rnk cardCombo count
                    | Set.member card cardCombo && cardCombo `Set.isSubsetOf` deck = rnk + (fromIntegral count) * (1.0 / (2.0 ^ Set.size cardCombo))
                    | otherwise = rnk