#!/usr/bin/env runhaskell
{-# LANGUAGE TypeFamilies #-}

module Main where
import System.IO (getContents)
import Text.Regex.PCRE ((=~))
import System.Process (runCommand)
import Control.Monad (forM_)

newtype Regex = Regex { toString :: String }
data Message = Unit Int | Integration Int | Other String deriving (Show)
type Scanner a = String -> ScanData a
type Analyzer a = ScanData a -> Maybe Message
type Behavior = [String -> Maybe Message]

class Scannable a where
  type ScanData a :: *
  scanWith :: a -> String -> ScanData a

instance Scannable Regex where
  type ScanData Regex = [[String]]
  scanWith r = (=~ toString r)

---------------------------------------------------------

parseMessage :: String -> Behavior -> Maybe Message
parseMessage str [] = Nothing
parseMessage str (a:as) = case a str of
                            Just msg -> Just msg
                            Nothing -> parseMessage str as

plural :: Int -> String
plural n = if n == 1 then "" else "s"

format :: Message -> String
format (Unit n) = "Unit tests complete.. " ++ show n ++ " failure" ++ plural n
format (Integration n) = "Integration tests complete.. " ++ show n ++ " failure" ++ plural n
format (Other s) = s

---------------------------------------------------------

scanUnitTests = scanWith $ Regex "([0-9]+) failures"
analyzeUnitTests matches | null matches = Nothing
                         | otherwise = (Just . Unit . read) $ matches !! 0 !! 1


scanIntegrationTests = scanWith $ Regex "[0-9]+ steps.*\\((?:([0-9]+) failed)?"
analyzeIntegrationTests matches | null matches = Nothing
                                | otherwise = case matches !! 0 !! 1 of
                                                "" -> (Just . Integration) 0
                                                n  -> (Just . Integration . read) n

scanMigrationStart = scanWith $ Regex "Loading db/seeds\\.sql"
analyzeMigrationStart matches | null matches = Nothing
                              | otherwise = (Just . Other) "Beginning migrations."

scanUnitTestStart = scanWith $ Regex "rspec"
analyzeUnitTestStart matches | null matches = Nothing
                             | otherwise = (Just . Other) "Migrations complete. Beginning unit tests."


defaultBehavior :: Behavior
defaultBehavior = [analyzeUnitTests . scanUnitTests,
                   analyzeIntegrationTests . scanIntegrationTests,
                   analyzeMigrationStart . scanMigrationStart,
                   analyzeUnitTestStart . scanUnitTestStart]

---------------------------------------------------------

vocalize :: Message -> IO ()
vocalize message = do runCommand $ "say -v Sangeeta " ++ format message
                      return ()

main = do input <- getContents
          forM_ (lines input) $ \line ->
            case parseMessage line defaultBehavior of
              Just message -> vocalize message
              Nothing -> return ()
          vocalize $ Other "Operation complete."
