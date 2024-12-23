module Parsing
    ( module X
    , Parser
    , Text
    , parseOrFail
    , manyOptional
    , foldrMany
    , StatefulParser
    , parseOrFailStateful
    )
    where

import Text.Megaparsec as X hiding (State)
import Text.Megaparsec.Char as X
import Data.Void
import Data.Text
import qualified Data.Text.IO as T (readFile)
import Control.Applicative hiding (many, some)
import Control.Monad.State.Strict as X
import Control.Monad (MonadPlus(..), (<=<))
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

manyOptional :: MonadParsec e s m => m a -> m [a]
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

foldrMany :: MonadPlus m => (a -> b -> b) -> b -> m a -> m b
foldrMany combine z p = go id where
    go f = do
        mx <- optional p
        case mx of
          Nothing -> pure $ f z
          Just x -> go (f . combine x)

-- | effects will happen right to left. writing one that does the effects left
--   to right seems tricky. especially if you wanted to do the effects left to right
--   but then combine the results right to left
foldrManyM :: MonadPlus m => (a -> b -> m b) -> b -> m a -> m b
foldrManyM combine z p = go pure where
    go f = do
        mx <- optional p
        case mx of
          Nothing -> f z
          Just x -> go (f <=< combine x)
