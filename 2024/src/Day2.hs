{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Parsing
import qualified Parsing.Lexer as L
import Control.Applicative hiding (many, some)
import Control.Comonad
import Data.Foldable (foldl')
import Data.Zipper

main :: IO ()
main = do
    reports <- parseOrFail parseReports "inputs/day2.txt"
    print $ foldl' (\ n rep -> n + (fromEnum . isSafe . unReport) rep) 0 reports
    print $ foldl' (\ n rep -> n + (fromEnum . isAlmostSafe) rep) 0 reports

newtype Report = Report { unReport :: [Int] }

isSafe :: [Int] -> Bool
isSafe vals =
    let changes = zip vals $ drop 1 vals
        step !(!sign, !good) (n, n1) =
            let delta = n1 - n
                absDel = abs delta
                sigDel = signum delta
                stillGood = good && absDel <= 3 && absDel >= 1 && (sigDel == sign || sign == 0)
             in (signum delta, stillGood)
     in snd $ foldl' step (0, True) changes

isAlmostSafe :: Report -> Bool
isAlmostSafe = or . extend (isSafe . dropFocus) . fromList . unReport

parseReports :: Parser [Report]
parseReports = many $ Report <$> some (L.lex L.decimal) <* eol
