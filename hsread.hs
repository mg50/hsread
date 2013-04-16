#!/usr/bin/env runhaskell
{-# LANGUAGE TypeFamilies #-}

module Main where
import System.IO (getContents)
import Text.Regex.PCRE ((=~))
import System.Process (runCommand, waitForProcess)
import Control.Monad (forM_)
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Directory (getCurrentDirectory)
import Data.List.Split (splitOn)
import Control.Concurrent.Chan
---------------------------------------------------------

data Context = Context { folder :: String } deriving (Show)

newtype Regex = Regex { toString :: String }
data Message = Unit Int
             | Integration Int
             | BeginningMigrations
             | FinishedMigrations
             | Other String
             deriving (Show)

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

format :: (Monad a) => Message -> ReaderT Context a String
format (Unit n)            = do f <- asks folder
                                return $ f ++ " unit tests complete.. " ++
                                         show n ++ " failure" ++ plural n
format (Integration n)     = do f <- asks folder
                                return $ f ++ " integration tests complete.. " ++
                                         show n ++ " failure" ++ plural n
format BeginningMigrations = do f <- asks folder
                                return $ "Beginning " ++ f ++ " migrations."
format FinishedMigrations  = do f <- asks folder
                                return $ f ++ " migrations complete. Beginning " ++ f ++ " unit tests."
format (Other s)           = return s

---------------------------------------------------------

scanUnitTests = scanWith $ Regex "([0-9]+) failure"
analyzeUnitTests matches | null matches = Nothing
                         | otherwise = (Just . Unit . read) $ matches !! 0 !! 1


scanIntegrationTests = scanWith $ Regex "[0-9]+ steps.*\\((?:([0-9]+) failed)?"
analyzeIntegrationTests matches | null matches = Nothing
                                | otherwise = case matches !! 0 !! 1 of
                                                "" -> (Just . Integration) 0
                                                n  -> (Just . Integration . read) n

scanMigrationStart = scanWith $ Regex "Loading db/seeds\\.sql"
analyzeMigrationStart matches | null matches = Nothing
                              | otherwise = Just BeginningMigrations

scanUnitTestStart = scanWith $ Regex "-S rspec"
analyzeUnitTestStart matches | null matches = Nothing
                             | otherwise = Just FinishedMigrations


defaultBehavior :: Behavior
defaultBehavior = [analyzeUnitTests . scanUnitTests,
                   analyzeIntegrationTests . scanIntegrationTests,
                   analyzeMigrationStart . scanMigrationStart,
                   analyzeUnitTestStart . scanUnitTestStart]

---------------------------------------------------------

vocalize :: (MonadIO a) => Message -> ReaderT Context a ()
vocalize message = do msg <- format message
                      pid <- liftIO $ runCommand $ "say -v Sangeeta " ++ msg
                      liftIO $ waitForProcess pid
                      return ()

getContext :: IO Context
getContext = do cwd <- getCurrentDirectory
                let folder = last $ splitOn "/"  cwd
                return $ Context{folder = folder}

vocalizeLoop :: Chan (Maybe Message) -> MVar () -> IO ()
vocalizeLoop chan done = do context <- getContext
                            t <- readChan chan
                            case t of
                              Just message -> do runReaderT (vocalize message) context
                                                 vocalizeLoop chan done
                              Nothing -> putMVar done ()

main :: IO ()
main = do input <- getContents
          messageQueue <- newChan
          done <- newEmptyMVar
          forkIO $ vocalizeLoop messageQueue done
          forM_ (lines input) $ \line ->
            case parseMessage line defaultBehavior of
              Just message -> writeChan messageQueue (Just message)
              Nothing -> return ()
          writeChan messageQueue Nothing
          takeMVar done
