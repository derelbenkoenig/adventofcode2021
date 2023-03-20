{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Day9 where

import Control.Applicative hiding (many, some)
import Control.Monad (MonadPlus(..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Foldable (foldl')

runSolution n fp = do
    input <- T.readFile fp
    motions <- parseOrFail (many parseMotion) fp input
    case n of
        1 -> do 
            print $ solution1 motions
        2 -> do
            undefined
        _ -> fail "problem must be 1 or 2"

solution1 :: [(Coord, Int)] -> Int
solution1 = S.size .
    snd .
    foldlWithIterations'
        (\(rope, positions) motion ->
            applyMotionCollectingTail positions motion rope)
       (Rope (V2 0 0) (V2 0 0), S.singleton (V2 0 0))

type Coord = V2 Int

data Rope = Rope 
    Coord -- ^ head
    Coord -- ^ tail
    deriving Show

data V2 a = V2 a a
    deriving (Show, Eq, Ord)

instance Functor V2 where
    fmap f (V2 x y) = V2 (f x) (f y)

instance Applicative V2 where
    pure x = V2 x x
    V2 f g <*> V2 x y = V2 (f x) (g y)

-- I know this means something different in other packages but whatever
(<+>) :: Num a => V2 a -> V2 a -> V2 a
(<+>) = liftA2 (+)

nTimes :: Int -> (a -> a) -> a -> a
nTimes = go id where
    go k n f x = if n <= 0 then k x else go (f . k) (n - 1) f x

catchUp :: Coord -> Coord -> Coord
catchUp (V2 hx hy) (V2 tx ty) =
    let dx = hx - tx
        dy = hy - ty
     in if abs dx <= 1 && abs dy <= 1 
           then V2 tx ty
           else V2 (tx + signum dx) (ty + signum dy)

-- Coord is assumed to be a unit vector either straight up/down/left/right
applyMotion :: Coord -> Rope -> Rope
applyMotion motion (Rope h t) =
    let newHead = h <+> motion
     in Rope newHead (catchUp newHead t)

collecting :: (e -> s -> s) -> s -> (a -> e) -> a -> (e, s)
collecting f a g x = let y = g x in (y, f y a)

applyMotionCollectingTail :: S.Set Coord -> Coord -> Rope -> (Rope, S.Set Coord)
applyMotionCollectingTail s motion rope =
    collecting (\(Rope _ t) -> S.insert t) s (`applyMotion` rope) motion

foldlWithIterations' :: Foldable f => (b -> a -> b) -> b -> f (a, Int) -> b
foldlWithIterations' f = foldl' (\acc (x, n) -> nTimes n (`f` x) acc)

symbol :: T.Text -> Parser T.Text
symbol = L.symbol hspace
lexeme = L.lexeme hspace

parseMotion :: Parser (Coord, Int)
parseMotion = do
    c <- lexeme (oneOf ("LURD" :: [Char])) >>= charToCoord
    n <- lexeme L.decimal
    eol
    return (c, n)

charToCoord :: Alternative m => Char -> m Coord
charToCoord 'L' = pure (V2 (-1) 0)
charToCoord 'U' = pure (V2 0 1)
charToCoord 'R' = pure (V2 1 0)
charToCoord 'D' = pure (V2 0 (-1))
charToCoord _ = empty
