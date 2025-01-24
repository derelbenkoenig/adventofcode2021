{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq(..))

import Parsing

main :: IO ()
main = do
    segments <- parseOrFail parseDiskSegments "inputs/day9.txt"
    let stDisk = diskFromDiskmap segments
    print $ runST $ stDisk >>= compactDisk >>= diskChecksum

    print $ labeledMapChecksum $ compactDiskFilewise $ labelSegments segments

data DiskSegment = FileSeg Int | FreeSeg Int
    deriving (Eq, Show)

numBlocks :: DiskSegment -> Int
numBlocks (FileSeg x) = x
numBlocks (FreeSeg x) = x

parseDiskSegments :: Parser [DiskSegment]
parseDiskSegments =
    zipWith ($) (cycle [FileSeg, FreeSeg])
        <$> (many (digitToInt <$> digitChar) <* eol)


diskFromDiskmap :: [DiskSegment] -> ST s (STUArray s Int Int)
diskFromDiskmap diskmap = do
    let totalSize = foldl' ((. numBlocks) . (+)) 0 diskmap
    diskArr <- newArray (0, totalSize - 1) (-1)
    let go ix fileId segments =
            case segments of
              [] -> pure diskArr
              (FileSeg n):rest ->
                  if n <= 0
                     then go ix (fileId + 1) rest
                     else do
                         writeArray diskArr ix fileId
                         go (ix + 1) fileId (FileSeg (n - 1):rest)
              (FreeSeg n):rest ->
                  if n <= 0
                     then go ix fileId rest
                     else go (ix + 1) fileId (FreeSeg (n - 1):rest)
    go 0 0 diskmap

compactDisk :: STUArray s Int Int -> ST s (STUArray s Int Int)
compactDisk arr = getBounds arr >>= uncurry go
    where
        go ixL ixR =
            if ixR <= ixL
               then pure arr
               else do
                   currFileId <- readArray arr ixL
                   rightFileId <- readArray arr ixR
                   if
                      | currFileId /= (-1) -> go (ixL + 1) ixR
                      | rightFileId == (-1) -> go ixL (ixR - 1)
                      | otherwise -> do
                          writeArray arr ixL rightFileId
                          writeArray arr ixR (-1)
                          go (ixL + 1) (ixR - 1)


diskChecksum :: STUArray s Int Int -> ST s Int
diskChecksum arr = foldl' foo 0 <$> getAssocs arr where
    foo acc (ix, fileId) =
        if fileId >= 0
           then acc + ix * fileId
           else acc

data LabeledSegment = FileSegLabeled {-# UNPACK #-} !Int {-# UNPACK #-} !Int | FreeSegUnlabeled {-# UNPACK #-} !Int
    deriving (Eq, Show)

type LabeledDiskmap = Seq LabeledSegment

snocSeg :: LabeledDiskmap -> LabeledSegment -> LabeledDiskmap
(segs :|> FreeSegUnlabeled x) `snocSeg` FreeSegUnlabeled y = segs :|> FreeSegUnlabeled (x + y)
segs `snocSeg` fs = segs :|> fs

infixl 5 `snocSeg`

consSeg :: LabeledSegment -> LabeledDiskmap -> LabeledDiskmap
FreeSegUnlabeled y `consSeg` (FreeSegUnlabeled x :<| segs) = FreeSegUnlabeled (x + y) :<| segs
seg `consSeg` segs = seg :<| segs

infixr 5 `consSeg`

labelSegments :: [DiskSegment] -> LabeledDiskmap
labelSegments = go 0 id
    where
        go _ k [] = k Empty
        go n k ((FileSeg x):segs) = go (n + 1) (k . (FileSegLabeled n x `consSeg`)) segs
        go n k ((FreeSeg x):segs) = go n (k . (FreeSegUnlabeled x `consSeg`)) segs

compactDiskFilewise :: LabeledDiskmap -> LabeledDiskmap
compactDiskFilewise allSegs = go (lastFileId allSegs) allSegs where
    go :: Int -> LabeledDiskmap -> LabeledDiskmap
    go i segs | i <= 0 = segs
              | otherwise = moveIthFile i segs (go (i - 1))

    moveIthFile :: Int -> LabeledDiskmap -> (LabeledDiskmap -> LabeledDiskmap) -> LabeledDiskmap
    moveIthFile _ Empty k = k Empty
    moveIthFile i (segs :|> seg@(FreeSegUnlabeled _)) k = moveIthFile i segs (k . (`snocSeg` seg))
    moveIthFile i m@(segs :|> seg@(FileSegLabeled n x)) k
      -- | n == i = fromMaybe m $ fmap (`snocSeg` FreeSegUnlabeled x) $ tryInsertInto segs seg
      | n == i = runYMaybe (tryInsertInto segs seg) (`snocSeg` FreeSegUnlabeled x) (k m) k
      | otherwise = moveIthFile i segs (k . (:|> seg))

    tryInsertInto :: LabeledDiskmap -> LabeledSegment -> YMaybe LabeledDiskmap
    tryInsertInto Empty _ = yNothing
    tryInsertInto _ (FreeSegUnlabeled _) = error "tryInsertInto: pointless to insert free segment"
    tryInsertInto (h@(FileSegLabeled _ _) :<| segs) seg = consSeg h <$> tryInsertInto segs seg
    tryInsertInto (h@(FreeSegUnlabeled x) :<| segs) seg@(FileSegLabeled _ y)
      | y == x = yJust $ seg `consSeg` segs
      | y < x = yJust $ seg `consSeg` FreeSegUnlabeled (x - y) `consSeg` segs
      | otherwise = consSeg h <$> tryInsertInto segs seg

-- covariant Yoneda of Church encoded Maybe
newtype YMaybe a = YMaybe { runYMaybe :: forall b c. (a -> b) -> c -> (b -> c) -> c }

lowerYMaybe :: YMaybe a -> Maybe a
lowerYMaybe m = runYMaybe m id Nothing Just

-- lowerYMaybe (fmap f m)
-- = runYMaybe (fmap f m) id Nothing Just
-- = runYMaybe (YMaybe $ \ k n j -> runYMaybe m (k . f) n j) id Nothing Just
-- = (\ k n j -> runYMaybe m (k . f) n j) id Nothing Just
-- = (\ k n -> runYMaybe m (k . f) n Just) id Nothing
-- = (\ k -> runYMaybe m (k . f) Nothing Just) id
-- = runYMaybe m (id . f) Nothing Just
-- = runYMaybe m f Nothing Just

instance Functor YMaybe where
    fmap f m = YMaybe $ \ k n j -> runYMaybe m (k . f) n j

yJust :: a -> YMaybe a
yJust x = YMaybe $ \ k _ j -> j (k x)

yNothing :: YMaybe a
yNothing = YMaybe $ \ _ n _ -> n

lastFileId :: LabeledDiskmap -> Int
lastFileId Empty = error "lastFileId: no file segments in Sequence"
lastFileId (segs :|> FreeSegUnlabeled _) = lastFileId segs
lastFileId (_ :|> FileSegLabeled n _) = n

labeledMapChecksum :: LabeledDiskmap -> Int
labeledMapChecksum = go 0 0 where
    go _ acc Empty = acc
    go ix acc (FreeSegUnlabeled x :<| segs) = go (ix + x) acc segs
    go ix acc (fs@(FileSegLabeled _ x) :<| segs) = go (ix + x) (acc + contribution ix fs) segs

    -- contribution ix (FileSegLabeled n x) = ix * n + (ix + 1) * n + ... + (ix + x - 1) * n
    -- = (ix + (ix + 1) + ... + (ix + x - 1)) * n
    contribution ix (FileSegLabeled n x) = n * sumFromTo ix (ix + x - 1)
    contribution _ (FreeSegUnlabeled _) = 0 -- ^ unreachable, I'm pretty sure

-- https://math.stackexchange.com/a/2713687
sumFromTo :: Integral a => a -> a -> a
sumFromTo x y = (x + y) * (y - x + 1) `div` 2

-- for debugging purposes
showLdm :: LabeledDiskmap -> String
showLdm = foldr foo "" where
    foo seg s = (++ s) $ case seg of
                            FileSegLabeled n x -> replicate x (intToDigit (n `mod` 10))
                            FreeSegUnlabeled x -> replicate x '.'
