{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parsing
import qualified Parsing.Lexer as L
import Control.Applicative hiding (many, some)
import Data.Foldable (foldl')
import Data.Char (digitToInt)

main :: IO ()
main = do
    instructions <- parseOrFail parseInstructions "inputs/day3.txt"
    print $ sum $ fmap doInstr instructions

data Instruction = MulInstr !Int !Int

doInstr :: Instruction -> Int
doInstr (MulInstr a b) = a * b

parseInstructions :: Parser [Instruction]
parseInstructions =
    manyOptional $ MulInstr <$>
        (string "mul(" *> oneToThreeDigitNumber) <*>
        (char ',' *> oneToThreeDigitNumber <* char ')')

oneToThreeDigitNumber :: Parser Int
oneToThreeDigitNumber = do
    digits <- count' 1 3 digitChar
    pure $ foldl' (\n d -> n * 10 + digitToInt d) 0 digits
