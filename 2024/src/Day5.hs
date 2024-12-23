{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Parsing hiding (count)
import qualified Parsing.Lexer as L
import Control.Monad (liftM2, void)
import Algebra.Graph.Relation
    ( Relation
    , transitiveClosure
    , transpose
    , edgeList
    , hasEdge)
import Algebra.Graph.Class as G
import Algebra.Graph.Export.Dot
import Data.List (sortBy, partition)

main :: IO ()
main = do
    (rules, updates) <- parseOrFail parseInput "inputs/day5.txt"
    -- writeFile "rules.dot" $ exportViaShow rules
    -- putStrLn "updates"
    -- mapM_ print updates
    let (illegalUpdates, legalUpdates) = partition (updateIsIllegal rules) updates
    print $ sum $ fmap middle legalUpdates
    print $ sum $ fmap (middle . makeLegal rules) illegalUpdates

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

int :: Parser Int
int = L.lex L.decimal

parseInput :: Parser (Relation Int, [[Int]])
parseInput = do
    rules <- parseOrderingRules
    void eol
    updates <- parseUpdates
    pure (rules, updates)

parseOrderingRules :: Parser (Relation Int)
parseOrderingRules =
    foldrMany overlay G.empty $ liftM2 edge (int <* single '|') (int <* eol)

parseUpdates :: Parser [[Int]]
parseUpdates = many $ int `sepBy` single ',' <* eol

-- | an update is legal if it does not violate any rules.
--   That is, the graph does not contain an edge that connects a pair of vertices
--   in the opposite order than they are in the list
updateIsIllegal :: Relation Int -> [Int] -> Bool
updateIsIllegal rules = any (flip (uncurry hasEdge) rules) . illegalEdges

illegalEdges :: [Int] -> [(Int, Int)]
illegalEdges = edgeList . transitiveClosure . transpose . clique

makeLegal :: Relation Int -> [Int] -> [Int]
makeLegal rules = sortBy $ \x y ->
    if
       | hasEdge x y rules -> LT
       | hasEdge y x rules -> GT
       | otherwise -> EQ
