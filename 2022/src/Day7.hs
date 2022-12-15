{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Control.Applicative hiding (many, some)
import Control.Monad (liftM2, mfilter, void)
import Data.Char
import Data.Constraint
import Data.Foldable (foldl', foldMap')
import Data.Maybe (catMaybes)
import Data.Kind (Type)
import Data.Semigroup (Sum(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Solutions
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runSolution n fp = do
    input <- T.readFile fp
    fileTree <- parseOrFail parseFileTreeFromShellSession fp input
    let dirSizes = allDirectorySizes fileTree
    if n == 1
        then print $ sum $ filter (<= 100000) dirSizes
        else do
            let spaceUsed = maximum dirSizes
                totalCapacity = 70000000
                freeSpaceNeeded = 30000000
                freesEnough n = spaceUsed - n <= totalCapacity - freeSpaceNeeded
                candidates = filter freesEnough dirSizes
                victimDirectory = minimum candidates
            print victimDirectory


data Some1 (f :: k -> Type) where
    Some1 :: f a -> Some1 f

instance forall f. (forall a. Show (f a)) => Show (Some1 f) where
    showsPrec d (Some1 x) = showParen (d > 10) $ showString "Some1 " . showsPrec 11 x

data CmdType = Cd | Ls
    deriving (Eq, Show)

data Cmd (a :: CmdType) where
    CdCmd :: DirSpec -> Cmd Cd
    LsCmd :: Cmd Ls
deriving instance Show (Cmd a)
deriving instance Eq (Cmd a)

data CmdOutput (a :: CmdType) where
    CdOutput :: CmdOutput Cd -- There's no output, but representing it as a unit
    LsOutput :: [Some1 FileListing] -> CmdOutput Ls
deriving instance Show (CmdOutput a)

data CmdAndOutput (a :: CmdType) = CmdAndOutput (Cmd a) (CmdOutput a)
    deriving (Show)

data DirSpec = DirName T.Text | ParentDir | RootDir
    deriving (Eq, Show)

data FileType = RegularFile | Directory
    deriving (Eq, Show)

data FileListing (a :: FileType) where
    RegularListing :: Int -> T.Text -> FileListing RegularFile
    DirListing :: T.Text -> FileListing Directory
deriving instance Show (FileListing a)
deriving instance Eq (FileListing a)

newtype ShellSession = ShellSession [Some1 CmdAndOutput]
    deriving (Show)

data FileNode (a :: FileType) where
    RegularNode :: T.Text -> Int -> FileNode RegularFile
    DirNode :: T.Text -> [Some1 FileNode] -> FileNode Directory
deriving instance Show (FileNode a)

totalFileSize :: FileNode a -> Int
totalFileSize fn = case fn of
    RegularNode _ s -> s
    DirNode _ children -> getSum $ foldMap' (\(Some1 x) -> Sum $ totalFileSize x) children

-- yeah it would be nice to learn recursion schemes to figure out a nicer way to do this
allDirectorySizes :: FileNode a -> [Int]
allDirectorySizes fn = fst $ go fn where
    go :: (forall a. FileNode a -> ([Int], Int))
    go (RegularNode _ s) = ([], s)
    go (DirNode _ children) =
        let childResults = map (\(Some1 x) -> go x) children
            (ds, Sum size) = foldMap' (\(xs, i) -> (xs , Sum i)) childResults
            in (size:ds, size)

symbol = L.symbol hspace
lexeme = L.lexeme hspace

isFilenameChar c = isAlpha c || c == '.'

parseFileName :: Parser T.Text
parseFileName = (lexeme $ takeWhile1P (Just "filename (letters and '.')") isFilenameChar)

parseDirSpec :: Parser DirSpec
parseDirSpec =
    RootDir <$ symbol "/"
    <|> ParentDir <$ symbol ".."
    <|> DirName <$> parseFileName

altSome1 :: forall {k}
                   (m :: Type -> Type)
                   (f :: k -> Type) 
                   (a :: k)
                   (b :: k).
                       Alternative m
                       => m (f a)
                       -> m (f b)
                       -> m (Some1 f)
altSome1 ma mb = fmap Some1 ma <|> fmap Some1 mb

cmdLine :: Parser (Cmd a) -> Parser (Cmd a)
cmdLine p = symbol "$" *> p <* eol

parseCdCmd :: Parser (Cmd Cd)
parseCdCmd = cmdLine $ symbol "cd" *> (CdCmd <$> lexeme parseDirSpec)

parseLsCmd :: Parser (Cmd Ls)
parseLsCmd = cmdLine $ LsCmd <$ symbol "ls"

parseRegFileListing :: Parser (FileListing RegularFile)
parseRegFileListing = RegularListing <$> lexeme L.decimal <*> parseFileName

parseDirListing :: Parser (FileListing Directory)
parseDirListing = DirListing <$> (symbol "dir" *> parseFileName)

parseCommandAndOutput :: Parser (Cmd a) -> Parser (CmdOutput a) -> Parser (CmdAndOutput a)
parseCommandAndOutput = liftM2 CmdAndOutput

parseCdOutput :: Parser (CmdOutput Cd)
parseCdOutput = pure CdOutput -- don't actually parse anything

parseCdAndOutput :: Parser (CmdAndOutput Cd)
parseCdAndOutput = parseCommandAndOutput parseCdCmd parseCdOutput

parseLsOutput :: Parser (CmdOutput Ls)
parseLsOutput = fmap LsOutput (parseListing `endBy` eol) where
    parseListing = (parseRegFileListing `altSome1` parseDirListing)

parseLsAndOutput :: Parser (CmdAndOutput Ls)
parseLsAndOutput = parseCommandAndOutput parseLsCmd parseLsOutput

parseShellSession :: Parser ShellSession
parseShellSession = ShellSession <$> many (try parseCdAndOutput `altSome1` parseLsAndOutput)

parseFileTreeFromShellSession :: Parser (FileNode Directory)
parseFileTreeFromShellSession = do
    dirName <- parseCdNonParent
    void parseLsCmd
    LsOutput entries <- parseLsOutput
    let regularChildren = (map (\(RegularListing size name) -> RegularNode name size)
                           . catMaybes
                           . map asRegularListing) entries
    dirChildren <- many $ try parseFileTreeFromShellSession
    parseCdParent <|> eof
    return $ DirNode dirName (map Some1 regularChildren ++ map Some1 dirChildren)

asRegularListing :: Some1 FileListing -> Maybe (FileListing RegularFile)
asRegularListing (Some1 fl) = case fl of
    DirListing _ -> Nothing
    x@(RegularListing _ _) -> Just x

parseCdParent :: Parser ()
parseCdParent = label "cd .." $ try $ do
    CdCmd x <- parseCdCmd
    if x == ParentDir
        then mempty
        else pure ()

parseCdNonParent :: Parser T.Text
parseCdNonParent = label "cd other than .." $ try $ do
    CdCmd x <- parseCdCmd
    case x of
        ParentDir -> mempty
        DirName t -> return t
        RootDir -> return "/"
