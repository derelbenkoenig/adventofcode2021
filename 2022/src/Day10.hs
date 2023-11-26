{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use id" #-}
module Day10 where

import Control.Applicative hiding (many, some)
-- import Control.Monad.Coroutine
-- import Control.Monad.Coroutine.SuspensionFunctors
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runSolution :: (Eq a, Num a) => a -> FilePath -> IO ()
runSolution n fp = do
    input <- T.readFile fp
    instructions <- parseOrFail (many (parseInstruction <* eol)) fp input
    case n of
        1 -> do 
            let machineStates =
                    concat $ scanl execInstr [initialMachine] instructions
            print machineStates
            let strengths = relevantStrengths machineStates
            print strengths
            print $ sum strengths
        2 -> do
            undefined instructions
        _ -> fail "problem must be 1 or 2"

data Instruction = Noop | Addx Int
    deriving Show

data Machine = Machine { cycleNo :: Int, xReg :: Int }
    deriving Show

signalStrength :: Machine -> Int
signalStrength (Machine cy xr) = cy * xr

relevantCycles :: [Int]
relevantCycles = [20, 60, 100, 140, 180, 220]

execInstr :: [Machine] -> Instruction -> [Machine]
execInstr states instr = go (last states) where
    go (Machine cy xr) =
        case instr of
          Noop -> [Machine (cy+1) xr]
          Addx n -> [Machine (cy+1) xr, Machine (cy+2) (xr+n)]

execInstr' :: Machine -> Instruction -> ([Machine], Machine)
execInstr' state instr = go state where
    go (Machine cy xr) =
        case instr of
          Noop -> ([Machine (cy+1) xr], Machine (cy+1) xr)
          Addx n ->
              ([Machine (cy+1) xr, Machine (cy+2) xr], Machine (cy+2) (xr+n))


-- yieldStates :: Monad m
--             => Machine
--             -> Coroutine (Yield Instruction) m ()
--             -> Coroutine (Yield Machine) m ()
-- yieldStates = concatYields . mapSuspension (\(Yield x y) -> Yield (execInstr' x) y)


relevantStrengths :: [Machine] -> [Int]
relevantStrengths = mapMaybe getRelevantStrength where
    getRelevantStrength (Machine cy xr) =
        if cy `elem` relevantCycles
           then Just $ cy * xr
           else Nothing

symbol :: T.Text -> Parser T.Text
symbol = L.symbol hspace

initialMachine :: Machine
initialMachine = Machine 0 1

parseInstruction :: Parser Instruction
parseInstruction =
    Noop <$ symbol "noop"
    <|> Addx <$> (symbol "addx" *> L.signed hspace L.decimal)

parseInstructions :: Parser [Instruction]
parseInstructions = parseInstruction `sepEndBy` eol

newtype YMaybe a = YMaybe { runYMaybe :: forall r. (a -> r) -> r -> r }

instance Functor YMaybe where
    fmap f (YMaybe g) = YMaybe (\k r -> g (k . f) r)

instance Applicative YMaybe where
    pure a = YMaybe (\k _ -> k a)
    YMaybe f <*> YMaybe g = YMaybe $ \k r -> f (\a -> g (k . a) r) r

instance Monad YMaybe where
    return = pure
    YMaybe f >>= g = YMaybe $ \k r -> f ((\ (YMaybe h) -> h k r) . g) r

joinYMaybe :: forall a. YMaybe (YMaybe a) -> YMaybe a
joinYMaybe (YMaybe f) = YMaybe $ f ( \(YMaybe g) -> g) (const id)

ymhelper :: (forall r. (YMaybe a -> r) -> r -> r)
         -> (forall r. ((forall r1. (a -> r1) -> r1 -> r1) -> r) -> r -> r)
ymhelper f k r = f (\(YMaybe g) -> k g) r

yMaybeToMaybe :: YMaybe a -> Maybe a
yMaybeToMaybe (YMaybe f) = f Just Nothing

yMaybeFromMaybe :: Maybe a -> YMaybe a
yMaybeFromMaybe ma = YMaybe $ \k r -> maybe r k ma
