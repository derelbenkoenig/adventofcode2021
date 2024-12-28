{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Parsing
import qualified Parsing.Lexer as L
import Data.List (foldl')
import Control.Monad (void)

main :: IO ()
main = do
    potEqns <- parseOrFail (many parsePotEqn) "inputs/day7.txt"
    mapM_ (\ usingOps -> print $ sum $ fmap (\ Eqn{..} -> tgtVal) $ potEqns >>= \ peq ->
        take 1 $ filter satisfied $ findEqn usingOps peq) [[Add, Mul], [Add, Mul, Cat]]

data PotEqn = PotEqn { tgtVal :: Int, arg1 :: Int, args :: [Int] }
    deriving (Eq, Show)

data Op = Add | Mul | Cat
    deriving (Eq, Show, Ord, Enum, Bounded)

doOp :: Op -> Int -> Int -> Int
doOp Add = (+)
doOp Mul = (*)
doOp Cat = \ x y -> read (show x ++ show y)

data Eqn = Eqn { tgtVal :: Int, arg1 :: Int, ops :: [(Op, Int)] }
    deriving (Eq, Show)

actualVal :: Eqn -> Int
actualVal Eqn{..} = foldl' (\ acc (op, arg) -> doOp op acc arg) arg1 ops

satisfied :: Eqn -> Bool
satisfied e@Eqn{tgtVal} = tgtVal == actualVal e

findEqn :: [Op] -> PotEqn -> [Eqn]
findEqn usingOps PotEqn{..} = do
    ops' <- traverse (\ n -> fmap (,n) usingOps) args
    pure $ Eqn tgtVal arg1 ops'

parsePotEqn :: Parser PotEqn
parsePotEqn = do
    tgt <- L.decimal <* char ':' <* hspace
    arg1' <- L.decimal <* hspace
    restArgs <- many $ L.decimal <* hspace
    void eol
    pure $ PotEqn tgt arg1' restArgs
