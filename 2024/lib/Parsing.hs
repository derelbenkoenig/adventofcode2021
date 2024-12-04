module Parsing (
    module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Parser,
    Text,
    parseOrFail
    )
    where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text
import qualified Data.Text.IO as T (readFile)

type Parser = Parsec Void Text

failOnParseError :: (MonadFail m,
                     VisualStream s,
                     TraversableStream s,
                     ShowErrorComponent e) =>
    Either (ParseErrorBundle s e) a -> m a
failOnParseError = either (fail . errorBundlePretty) pure

parseOrFail :: ShowErrorComponent e
                => Parsec e Text a
                -> FilePath
                -> IO a
parseOrFail parser fp = do
    input <- T.readFile fp
    failOnParseError $ parse parser fp input
