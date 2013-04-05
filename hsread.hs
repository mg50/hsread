#!/usr/bin/env runhaskell
{-# LANGUAGE TypeFamilies #-}

module Main where
import System.IO (getContents)
import Text.Regex.PCRE ((=~))
import System.Process (runCommand)
import Control.Monad (forM_)

newtype Regex = Regex { toString :: String }
data Message = Unit Int | Integration Int deriving (Show)
type Scanner a = String -> ScanData a
type ScanAnalyzer a = ScanData a -> Maybe Message
data ScanPair = RegexPair { toPair :: (Scanner Regex, ScanAnalyzer Regex)}
type MessageAnalyzer = [ScanPair]

class Scannable a where
  type ScanData a :: *
  scanWith :: a -> String -> ScanData a

instance Scannable Regex where
  type ScanData Regex = [[String]]
  scanWith r = (=~ toString r)

---------------------------------------------------------

parseMessage :: String -> MessageAnalyzer -> Maybe Message
parseMessage str [] = Nothing
parseMessage str (a:as) = let (scan, analyze) = toPair a
                          in case analyze (scan str) of
                            Just msg -> Just msg
                            Nothing -> parseMessage str as

format :: Message -> String
format (Unit n) = "Unit tests complete.. " ++ show n ++ " failures."
format (Integration n) = "Integration tests complete.. " ++ show n ++ " failures."

---------------------------------------------------------

scanUnitTests :: Scanner Regex
scanUnitTests = scanWith $ Regex "([0-9]+) failures"

analyzeUnitTests :: ScanAnalyzer Regex
analyzeUnitTests matches | null matches = Nothing
                         | otherwise = (Just . Unit . read) $ matches !! 0 !! 1


scanIntegrationTests :: Scanner Regex
scanIntegrationTests = scanWith $ Regex "[0-9]+ steps.*\\((?:([0-9]+)? failed)?"

analyzeIntegrationTests :: ScanAnalyzer Regex
analyzeIntegrationTests matches | null matches = Nothing
                                | otherwise = case matches !! 0 !! 1 of
                                                "" -> (Just . Integration) 0
                                                n  -> (Just . Integration . read) n

analyzer :: MessageAnalyzer
analyzer = [RegexPair (scanUnitTests, analyzeUnitTests),
            RegexPair (scanIntegrationTests, analyzeIntegrationTests)]

---------------------------------------------------------

main = do input <- getContents
          forM_ (lines input) $ \line ->
            case parseMessage line analyzer of
              Just message -> do runCommand $ "say -v Sangeeta " ++ format message
                                 return ()
              Nothing -> return ()
