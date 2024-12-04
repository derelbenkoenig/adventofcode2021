module Main (main) where

import Parsing
import qualified Parsing.Lexer as L
import Data.List (sort, foldl')
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
    (locs1, locs2) <- parseOrFail
        (parseListPair decimal decimal)
        "inputs/day1.txt"
    let sorted1 = sort locs1
        sorted2 = sort locs2
        diffs = zipWith ((abs .) . (-)) sorted1 sorted2
        total = sum diffs
    print total
    print $ similarityScore sorted1 sorted2

decimal :: Parser Int
decimal = L.lex L.decimal

parseListPair :: Parser a -> Parser b -> Parser ([a], [b])
parseListPair pA pB = go id id where
    go f g = do
        mPair <- optional ((,) <$> pA <*> pB <* eol)
        case mPair of
          Nothing -> pure (f [], g [])
          Just (a, b) -> go (f . (a :)) (g . (b :))

similarityScore :: [Int] -> [Int] -> Int
similarityScore as bs =
    let rcounts = foldl' (\ counts n -> Map.insertWith (+) n 1 counts) Map.empty bs
    in foldl' (\ total a -> total + maybe 0 (a *) (Map.lookup a rcounts)) 0 as
