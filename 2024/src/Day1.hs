module Main (main) where

import Parsing
import qualified Parsing.Lexer as L
import Data.List (sort)

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

decimal :: Parser Int
decimal = L.lex L.decimal

parseListPair :: Parser a -> Parser b -> Parser ([a], [b])
parseListPair pA pB = go id id where
    go f g = do
        mPair <- optional ((,) <$> pA <*> pB <* eol)
        case mPair of
          Nothing -> pure (f [], g [])
          Just (a, b) -> go (f . (a :)) (g . (b :))
