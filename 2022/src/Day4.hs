module Day4 where

import Control.Applicative hiding (many, some)
import Control.Monad (liftM2, unless, void)
import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runSolution n fp = do
    input <- T.readFile fp
    pairs <- parseOrFail parsePairs fp input
    p <- pairPred n
    print $ Day4.count (uncurry p) pairs

pairPred n = case n of
    1 -> return oneContainsOther
    2 -> return overlaps
    _ -> fail $ "there is no problem " <> show n

data SectionRange = SectionRange Int Int
    deriving (Eq, Show)

-- if they share either the left or right bound, then one must be within the other
--
-- otherwise, if the left bounds and right bounds are offset in the same direction,
-- then there is some non-overlap
--
oneContainsOther (SectionRange l1 r1) (SectionRange l2 r2) =
    case (compare l1 l2, compare r1 r2) of
        (EQ, _) -> True
        (_, EQ) -> True
        (x, y) | x == y -> False
        (_, _) -> True

overlaps (SectionRange l1 r1) (SectionRange l2 r2) = r2 >= l1 && r1 >= l2

parseSectionRange :: Parser SectionRange
parseSectionRange = label "\"x-y\" (where x <= y)" $ do
    l <- L.decimal
    void $ single '-'
    r <- L.decimal
    unless (l <= r) $ fail $ "bad range, " <> show l <> " is not <= " <> show r
    return $ SectionRange l r

parseLine :: Parser (SectionRange, SectionRange)
parseLine = liftM2 (,) (parseSectionRange <* void (single ',')) parseSectionRange

parsePairs :: Parser [(SectionRange, SectionRange)]
parsePairs = parseLine `sepEndBy` eol

count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldl' (\a x -> a + fromEnum (p x)) 0
