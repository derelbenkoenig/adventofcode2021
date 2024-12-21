module Parsing
    ( module X
    , Parser
    , Text
    , parseOrFail
    , manyOptional
    , StatefulParser
    , parseOrFailStateful
    )
    where

import Text.Megaparsec as X hiding (State)
import Text.Megaparsec.Char as X
import Data.Void
import Data.Text
import qualified Data.Text.IO as T (readFile)
import Control.Applicative as X hiding (many, some)
import Control.Monad.State.Strict as X
import Data.Functor.Identity

type Parser = Parsec Void Text

type StatefulParser s = ParsecT Void Text (StateT s Identity)

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

parseOrFailStateful :: ShowErrorComponent e
                        => ParsecT e Text (StateT s Identity) a
                        -> FilePath
                        -> s
                        -> IO (a, s)
parseOrFailStateful parser fp s = do
    input <- T.readFile fp
    let statefulParse = runParserT parser fp input
        (eitherResult, finalState) = runState statefulParse s
    either (fail . errorBundlePretty) (pure . (, finalState)) eitherResult

manyOptional :: Parser a -> Parser [a]
manyOptional p = go id where
    go f = do
        isAtEnd <- atEnd
        if isAtEnd
           then pure $ f []
           else do
               mx <- optional (try p)
               case mx of
                 Nothing -> anySingle >> go f
                 Just x -> go (f . (x :))
