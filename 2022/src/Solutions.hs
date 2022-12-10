{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Solutions where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Void
import Numeric.Natural
import qualified Text.Megaparsec as M

data ProblemNumber = ProblemNumber Natural Natural
    deriving (Eq, Show)

type Parser = M.Parsec Void T.Text

failOnParseError :: (MonadFail m,
                     M.VisualStream s,
                     M.TraversableStream s,
                     M.ShowErrorComponent e) =>
    Either (M.ParseErrorBundle s e) a -> m a
failOnParseError = either (fail . M.errorBundlePretty) return

parseOrFail parser fp input = failOnParseError $ M.parse parser fp input
