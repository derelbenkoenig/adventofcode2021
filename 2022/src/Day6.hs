{-# LANGUAGE TupleSections #-}

module Day6 where

import Control.Applicative hiding (many, some)
import Control.Monad (mfilter, replicateM, void)
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day3 (textToSet)
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runSolution n fp = do
    input <- T.readFile fp
    markerLen <- startMarkerLen n
    sopOffset <- fmap snd $ parseOrFail (findStartMarker markerLen) fp input
    print sopOffset

startMarkerLen :: (MonadFail m, Integral i) => i -> m Int
startMarkerLen 1 = pure sopLength
startMarkerLen 2 = pure somLength
startMarkerLen _ = fail "problem must be 1 or 2"

allDistinct :: T.Text -> Bool
allDistinct t = S.size (textToSet t) == T.length t

sopLength = 4
somLength = 14

parseStartMarker :: Int -> Parser T.Text
parseStartMarker n = mfilter (allDistinct) $ takeP (Just $ show n <> " distinct chars") n

-- attempt to parse n distinct chars
-- if it fails, discard 1 char and try again,
-- and increment the counter of how many chars we've had to parse through before
-- finding the match.
findStartMarker :: Int -> Parser (T.Text, Int)
findStartMarker n = go n where
    go l = fmap (, l) (try $ parseStartMarker n) <|>
        void anySingle *> go (l + 1)

