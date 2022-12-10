{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Solutions where

import Numeric.Natural
import qualified Text.Megaparsec as M
import qualified Data.Text.IO as T

data ProblemNumber = ProblemNumber Natural Natural
    deriving (Eq, Show)

failOnParseError :: (MonadFail m,
                     M.VisualStream s,
                     M.TraversableStream s,
                     M.ShowErrorComponent e) =>
    Either (M.ParseErrorBundle s e) a -> m a
failOnParseError = either (fail . M.errorBundlePretty) return

parseOrFail parser fp input = failOnParseError $ M.parse parser fp input
