{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parsing
import qualified Parsing.Lexer as L
import Data.Foldable (foldl')
import Data.Char (digitToInt)

main :: IO ()
main = do
    instructions <- parseOrFail parseInstructions "inputs/day3.txt"
    print $ sum $ fmap performInstr instructions
    print $ fst $ foldl'
        (\ !(!total, !doing) instr ->
            case instr of
              MulInstr x y -> (if doing then total + x * y else total, doing)
              DoInstr -> (total, True)
              DontInstr -> (total, False)
        )
        (0, True)
        instructions

data Instruction =
    MulInstr !Int !Int
      | DoInstr
      | DontInstr

performInstr :: Instruction -> Int
performInstr (MulInstr a b) = a * b
performInstr _ = 0

parseInstructions :: Parser [Instruction]
parseInstructions =
    manyOptional $ try parseMulInstr <|> try parseDoInstr <|> parseDontInstr

parseMulInstr :: Parser Instruction
parseMulInstr =
    MulInstr
    <$> (string "mul(" *> oneToThreeDigitNumber)
    <*> (char ',' *> oneToThreeDigitNumber <* char ')')

parseDoInstr :: Parser Instruction
parseDoInstr = DoInstr <$ string "do()"

parseDontInstr :: Parser Instruction
parseDontInstr = DontInstr <$ string "don't()"

oneToThreeDigitNumber :: Parser Int
oneToThreeDigitNumber = do
    digits <- count' 1 3 digitChar
    pure $ foldl' (\n d -> n * 10 + digitToInt d) 0 digits
