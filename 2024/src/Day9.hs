{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad.ST

import Data.Array.MArray
import Data.Array.ST
import Data.Char (digitToInt)
import Data.Foldable (foldl')

import Parsing

main :: IO ()
main = do
    segments <- parseOrFail parseDiskSegments "inputs/day9.txt"
    let stDisk = diskFromDiskmap segments
    print $ runST $ stDisk >>= compactDisk >>= diskChecksum

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
