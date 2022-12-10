module Day3 where

import Control.Applicative hiding (many, some)
import Control.Monad (unless, (<=<), replicateM)
import Data.Char
import Data.List (foldl1')
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char

runSolution 1 fp = do
    input <- T.readFile fp
    rucksacks <- parseOrFail parseRucksacks fp input
    priorities <- mapM ((priority <=< commonElement) . compartments) rucksacks
    print $ sum priorities

runSolution 2 fp = do
    input <- T.readFile fp
    groups <- parseOrFail parseElfGroups fp input
    print groups
    priorities <- mapM ((priority <=< commonElement) . map (textToSet . unRucksack)) groups
    print $ sum priorities

newtype Rucksack = Rucksack { unRucksack :: T.Text }
    deriving Show

parseRucksack :: Parser Rucksack
parseRucksack = Rucksack <$> takeWhile1P Nothing isAlpha

parseRucksacks :: Parser [Rucksack]
parseRucksacks = parseRucksack `sepEndBy` eol

parseElfGroup :: Parser [Rucksack]
parseElfGroup = replicateM 3 $ parseRucksack <* eol

parseElfGroups :: Parser [[Rucksack]]
parseElfGroups = many parseElfGroup

textToSet = S.fromList . T.unpack

compartments (Rucksack t) = map textToSet [l, r] where
    (l, r) = T.splitAt (T.length t `div` 2) t

commonElement sets = do
    let i = foldl1' S.intersection sets
    unless (S.size i == 1) $ fail $ "not exactly one element in common in sets " <> show sets
    return $ S.elemAt 0 i

priority c
    | isLower c = pure $ fromEnum c - fromEnum 'a' + 1
    | isUpper c = pure $ fromEnum c - fromEnum 'A' + 27
    | otherwise = fail $ "unexpected non-alpha character " <> [c]
