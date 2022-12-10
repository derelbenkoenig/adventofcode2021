module Day2 where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Data.Foldable (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char

runSolution 1 fp = do
    input <- T.readFile fp
    result <- execStrategy parseStrategyGuide1 score1 fp input
    print result

runSolution 2 fp = do
    input <- T.readFile fp
    result <- execStrategy parseStrategyGuide2 score2 fp input
    print result

execStrategy parseFunc scoreFunc fp txt = do
    rounds <- parseOrFail parseFunc fp txt
    return $ foldl' (\total (theirs, x) -> total + scoreFunc theirs x) 0 rounds

data RPS = Rock | Paper | Scissors
    deriving (Eq, Show, Enum)

-- ordering for enum is based on: add 1 to win, subtract 1 to lose (modulo 3)
data Outcome = Draw | Win | Lose
    deriving (Eq, Show, Enum)

versus :: RPS -> RPS -> Outcome
versus x y = case (fromEnum x - fromEnum y) `mod` 3 of
    0 -> Draw
    1 -> Win
    2 -> Lose

choiceForOutcome :: RPS -> Outcome -> RPS
choiceForOutcome theirs outcome = toEnum $ (fromEnum theirs + fromEnum outcome) `mod` 3

pointBonus rps = case rps of
    Rock -> 1
    Paper -> 2
    Scissors -> 3

outcomeBonus outcome = case outcome of
    Lose -> 0
    Draw -> 3
    Win -> 6

score1 theirs mine = pointBonus mine + outcomeBonus (mine `versus` theirs)
score2 theirs outcome =
    let mine = choiceForOutcome theirs outcome
        in pointBonus mine + outcomeBonus outcome

parseRPS :: Char -> Char -> Char -> Parser RPS
parseRPS r p s = Rock <$ single r <|> Paper <$ single p <|> Scissors <$ single s

parseTheirs :: Parser RPS
parseTheirs = parseRPS 'A' 'B' 'C'

parseMine1 :: Parser RPS
parseMine1 = parseRPS 'X' 'Y' 'Z'

parseMine2 :: Parser Outcome
parseMine2 = Lose <$ single 'X' <|> Draw <$ single 'Y' <|> Win <$ single 'Z'

parseRound :: Parser a -> Parser (RPS, a)
parseRound pMine = do
    theirs <- parseTheirs
    void hspace1
    mine <- pMine
    void hspace
    return (theirs, mine)


parseRound1 :: Parser (RPS, RPS)
parseRound1 = parseRound parseMine1

parseRound2 :: Parser (RPS, Outcome)
parseRound2 = parseRound parseMine2

parseStrategyGuide1 :: Parser [(RPS, RPS)]
parseStrategyGuide1 = parseRound1 `sepEndBy` eol

parseStrategyGuide2 :: Parser [(RPS, Outcome)]
parseStrategyGuide2 = parseRound2 `sepEndBy` eol
