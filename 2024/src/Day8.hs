
module Main (main) where

import Control.Monad (guard, forM_)
import Control.Monad.List.Trans

import Data.Array.IArray
import Data.Biapplicative
import Data.Grid
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Set (Set)

import Prelude hiding (head)
import Parsing
import Parsing.Grid

main :: IO ()
main = do
    cityMap <- parseOrFail parseCityMap "inputs/day8.txt"
    forM_ [findAntinodes, findAntinodesCorrected] $ \ f -> 
        print $ length $ Set.unions $ fmap (f (bounds $ cells cityMap)) $ freqGroups cityMap

parseCityMap :: Parser (Grid (Maybe Char))
parseCityMap = do
    rawMap <- parseCharGrid
    pure $ fmap freqFromChar rawMap

freqFromChar :: Char -> Maybe Char
freqFromChar '.' = Nothing
freqFromChar c = Just c

freqGroups :: Grid (Maybe Char) -> [NonEmpty (Int, Int)]
freqGroups = fmap (fmap fst) . NE.groupAllWith snd . catMaybes . fmap leftStrength . assocs . cells

leftStrength :: Functor f => (a, f b) -> f (a, b)
leftStrength (a, fb) = fmap (a,) fb

plusCoords, minusCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)
plusCoords = biLiftA2 (+) (+)
minusCoords = biLiftA2 (-) (-)

findAntinodes :: ((Int, Int), (Int, Int)) -> NonEmpty (Int, Int) -> Set (Int, Int)
findAntinodes bds coords = Set.fromList $ runList $ do
    c1 <- coordList
    c2 <- coordList
    guard $ c1 /= c2
    let cup = c2 `plusCoords` (c2 `minusCoords` c1)
    guard $ inRange bds cup
    pure cup

    where
        coordList = hoistList $ NE.toList coords

findAntinodesCorrected :: ((Int, Int), (Int, Int)) -> NonEmpty (Int, Int) -> Set (Int, Int)
findAntinodesCorrected bds coords = Set.fromList $ runList $ do
    c1 <- coordList
    c2 <- coordList
    guard $ c1 /= c2
    let diff = c2 `minusCoords` c1
        coordsUp = takeWhileInRange $ iterateListT (`plusCoords` diff) c2
        coordsDown = takeWhileInRange $ iterateListT (`minusCoords` diff) c1
    coordsUp <|> coordsDown

    where
        takeWhileInRange = takeWhileListT $ inRange bds
        coordList = hoistList $ NE.toList coords

