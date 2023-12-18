{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Problems where

import Control.Monad ((>=>))
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO (Handle, openFile, IOMode(ReadMode))

newtype Solution a = Solution { runSolution :: Handle -> IO a }

textSolution :: (Text -> IO a) -> Solution a
textSolution f = Solution $ T.hGetContents >=> f

instance Functor Solution where
    fmap f = Solution . (fmap f .) . runSolution

instance Applicative Solution where
    pure = Solution . const . pure
    Solution f <*> Solution x = Solution (\h -> f h <*> x h)

instance Monad Solution where
    return = pure
    Solution m >>= f = Solution $ \h -> m h >>= \a -> runSolution (f a) h

data Day a b = Day
    { 
        part1 :: Solution a,
        part2 :: Solution b
    }

getPartNum :: Int -> PrintableDay -> PrintableSolution
getPartNum x (PrintableDay (Day{..})) = case x of
              1 -> PrintableSolution part1
              2 -> PrintableSolution part2
              _ -> error "part number must be 1 or 2"

computeAndPrintAnswer :: PrintableSolution -> FilePath -> IO ()
computeAndPrintAnswer (PrintableSolution sol) fp = do
    h <- openFile fp ReadMode
    answer <- runSolution sol h
    print answer

data PrintableSolution where
    PrintableSolution :: Show a => Solution a -> PrintableSolution

data PrintableDay where
    PrintableDay :: (Show a, Show b) => Day a b -> PrintableDay
