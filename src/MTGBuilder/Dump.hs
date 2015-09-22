module MTGBuilder.Dump where

import MTGBuilder.Deck
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

dumpDeck :: Deck -> String
dumpDeck deck = intercalate "\n" lines
    where
        lines :: [String]
        lines = fmap (\(cardName, count) -> (show count) ++ " " ++ cardName) (Map.assocs getMap)
            where
                getMap :: Map String Int
                getMap = foldl f Map.empty deck
                    where
                        f = (\map card -> Map.insertWith (+) (name card) 1 map)