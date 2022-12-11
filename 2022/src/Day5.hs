{-# LANGUAGE RecordWildCards #-}

module Day5 where

import Control.Applicative hiding (many, some)
import Control.Monad (replicateM, void)
import Data.Array.IArray
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Text.IO as T
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runSolution n fp = do
    input <- T.readFile fp
    (cratesInput, commands) <- parseOrFail parseAllInput fp input
    move <- moveFn n
    let stacks = cratesFromInput cratesInput
        finalStacks = foldl'
                        (\ss Move{..} -> move quantity sourceCol destCol ss)
                        stacks
                        commands
        finalTops = elems $ fmap head finalStacks
    putStrLn finalTops

moveFn 1 = pure moveCrates
moveFn 2 = pure moveCrates2
moveFn _ = fail "problem can only be 1 or 2"

type Crate = Char
type CrateStack = [Crate]
type CrateStacks = Array Int CrateStack
type CratesInput = [[Maybe Crate]]
data MoveCommand = Move { quantity :: Int, sourceCol :: Int, destCol :: Int }
    deriving Show

transfer :: Int -- ^ How many to move
         -> [a] -- ^ source stack
         -> [a] -- ^ dest stack
         -> ([a], [a]) -- ^ the new value of both stacks
transfer 0 from   to = (from, to)
transfer n (x:xs) to = transfer (n - 1) xs (x:to)

moveCrates :: Int -- ^ quantity to move
           -> Int -- ^ source stack index
           -> Int -- ^ dest   stack index
           -> CrateStacks -- ^ stack group on which to apply the operation
           -> CrateStacks
moveCrates qty from to stacks =
    let sourceStack      = stacks ! from
        destStack        = stacks ! to
        (newSrc, newDst) = transfer qty sourceStack destStack
        in stacks // [(from, newSrc), (to, newDst)]

moveCrates2 :: Int -- ^ quantity to move
            -> Int -- ^ source stack index
            -> Int -- ^ dest   stack index
            -> CrateStacks -- ^ stack group on which to apply the operation
            -> CrateStacks
moveCrates2 qty from to stacks =
    let sourceStack      = stacks ! from
        destStack        = stacks ! to
        (toMove, newSrc) = splitAt qty sourceStack
        newDst = toMove ++ destStack
        in stacks // [(from, newSrc), (to, newDst)]

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

cratesFromInput :: CratesInput -> CrateStacks
cratesFromInput input =
    let columns = map catMaybes $ transpose input
        in listArray (1, length columns) columns

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> (char '[' *> upperChar <* char ']') <|>
             Nothing <$ replicateM 3 (char ' ')

parseRow :: Parser [Maybe Crate]
parseRow = parseCrate `sepBy` char ' '

-- the 'try' is required bc the parser for 3 spaces will get to the line with the
-- column numbers, consume a space and then fail
parseCrateInput :: Parser CratesInput
parseCrateInput = try parseRow `sepEndBy` eol 

parseColumnNumbers :: Parser [Int]
parseColumnNumbers = hspace *> (L.decimal `sepEndBy` hspace) <* eol

parseCommand :: Parser MoveCommand
parseCommand =
    Move
    <$> (string "move" *> hspace *> L.decimal <* hspace)
    <*> (string "from" *> hspace *> L.decimal <* hspace)
    <*> (string "to"   *> hspace *> L.decimal <* hspace)

parseAllInput :: Parser (CratesInput, [MoveCommand])
parseAllInput =
    (,)
    <$> (parseCrateInput <* parseColumnNumbers <* eol)
    <*> (parseCommand `sepEndBy` eol)
