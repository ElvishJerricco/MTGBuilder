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

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Tuple
import           MTGBuilder.Combination
import           MTGBuilder.Options
import           System.IO

data Card = MkCard {
    name        :: String,
    copy        :: Int,
    isSideboard :: Bool
} deriving (Eq, Ord)

instance Show Card where
    show c
        | isSideboard c = "SB: " ++ name c ++ " #" ++ show (copy c)
        | otherwise =               name c ++ " #" ++ show (copy c)

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
    interaction     :: Map (Set Card) Int,
    interactionSize :: Int,
    inputDecks      :: [Deck],
    inputCards      :: Deck
}

-- The return type of this function is a reader over IO so that verbosity can be read, and verbose messages can be printed
makeRanking :: Int -> [(String, Deck)] -> ReaderT Options IO Ranking
makeRanking size inputDecks = do
    Options {optVerbose=verbose} <- ask
    let inputs = fmap snd inputDecks
    rankDecks MkRanking { interaction=Map.empty, interactionSize=size, inputDecks=inputs, inputCards=Set.unions inputs } inputDecks
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
        lines = fmap line (Map.toList getMap)
            where
                line :: ((Bool, String), Int) -> String
                line ((isSide, cardName), count)
                    | isSide = "SB: " ++ show count ++ " " ++ cardName
                    | otherwise =        show count ++ " " ++ cardName
                getMap :: Map (Bool, String) Int
                getMap = foldl f Map.empty deck
                    where
                        f map card = Map.insertWith (+) (isSideboard card, name card) 1 map

dumpRanking :: Ranking -> String
dumpRanking ranking = intercalate "\n" lines
    where
        lines :: [String]
        lines = fmap (\(combo, count) -> show count ++ " : " ++ show combo) $ Map.toList $ interaction ranking

{-
Composition combines all the input decks.
Note: Although different copies of the same card are treated as different cards in this algorithm,
the same copies of the same card from different decks are treated as the same.
So when we union the decks, we are merely merging all the cards in all decks.
So if two decks each have 4 Bolts, we still only see 4 Bolts in the union.

Composing decks simply sorts the cards in the union by sortWithRanking,
then removes the lowest ranked card, then repeats until the deck is down to the provided size.
-}
composeDecks :: Ranking -> (Int, Int) -> ReaderT Options IO Deck
composeDecks ranking (mainSize, sideSize) =
    let startSize = foldl (\(m, s) c -> if isSideboard c then (m, s + 1) else (m + 1, s)) (0, 0) $ inputCards ranking
    in  compose startSize $ Set.unions $ inputDecks ranking
    where
        compose :: (Int, Int) -> Deck -> ReaderT Options IO Deck
        compose (main, side) cards
            | main <= mainSize && side <= sideSize = return cards
            | otherwise = do
                Options {optVerbose=verbose} <- ask
                when verbose $ liftIO $ hPrint stderr (Set.size cards)
                when verbose $ liftIO $ hPrint stderr (worstRank, worstCard)
                compose newSize (worstCard `Set.delete` cards)
            where
                newSize
                    | isSideboard worstCard = (main, side - 1)
                    | otherwise = (main - 1, side)
                (worstRank, worstCard) = head $ Set.toList sorted
                sorted = Set.filter filt $ sortWithRanking ranking cards
                filt (_, card)
                    | (side <= sideSize && isSideboard card) || (main <= mainSize && not (isSideboard card)) = False
                    | otherwise = True

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
                rank = fromIntegral count * 1.0 / (2.0 ^ Set.size combo)

{-
Addititive composition is similar to subtractive composition.
This new algorithm will be the new default, due to it's performance gains and added capabilities.

Rather than starting with the collective and working down,
start with nothing (or something) and work up.
That is, find the card that adds the most to the deck, and add that to it.

An advantage of this algorithm is that you can provide a starting state,
which allows you to specify cards you want the deck to be built around.
-}
composeAdditive :: Ranking -> (Int, Int) -> Deck -> ReaderT Options IO Deck
composeAdditive ranking (mainSize, sideSize) startDeck =
    let startState = foldl (\(m, s) c -> if isSideboard c then (m, s + 1) else (m + 1, s)) (0, 0) startDeck
    in  composeAdditive' startState startDeck
    where
        composeAdditive' :: (Int, Int) -> Deck -> ReaderT Options IO Deck
        composeAdditive' (main, side) deck
            | main >= mainSize && side >= sideSize = return deck
            | otherwise = do
                Options {optVerbose=verbose} <- ask
                when verbose $ liftIO $ hPrint stderr (Set.size deck)
                when verbose $ liftIO $ hPrint stderr (bestRank, bestCard)
                composeAdditive' newSize (bestCard `Set.insert` deck)
            where
                newSize
                    | isSideboard bestCard = (main, side + 1)
                    | otherwise = (main + 1, side)
                (bestRank, bestCard) = minimumBy (flip compare) (swap <$> Map.toList rankMap)
                rankMap = Map.filterWithKey filt $ Map.foldlWithKey rankCombo Map.empty $ interaction ranking
                filt card r
                    | (side >= sideSize && isSideboard card) || (main >= mainSize && not (isSideboard card)) = False
                    | otherwise = True
                rankCombo map combo count
                    | Set.size dif == 1 = Map.insertWith (+) (Set.elemAt 0 dif) rank map
                    | otherwise = map
                    where
                        dif = combo `Set.difference` deck
                        rank = fromIntegral count * 1.0 / (2.0 ^ Set.size combo)
