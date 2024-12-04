{-# LANGUAGE TypeOperators #-}

module Parsing.Lexer (
    module Text.Megaparsec.Char.Lexer,
    Parsing.Lexer.lex
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

lex :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lex = lexeme hspace
