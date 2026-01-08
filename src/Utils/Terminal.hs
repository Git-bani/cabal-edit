{-# LANGUAGE OverloadedStrings #-}
module Utils.Terminal 
  ( selectItems
  , toggleDashboard
  , selectPackages
  ) where

import Core.Types (PackageMetadata(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hSetBuffering, hSetEcho, stdin, BufferMode(..), stdout, hFlush)
import System.Console.ANSI
import Control.Monad (forM_)

-- | Select multiple items from a list (starts all True)
selectItems :: Text -> [Text] -> IO [Text]
selectItems title items = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ T.unpack title
  putStrLn "(Use arrow keys to move, Space to toggle, Enter to confirm)"
  selectedStates <- loop 0 (replicate (length items) True) items
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return [item | (state, item) <- zip selectedStates items, state]

-- | Select packages with rich metadata display
selectPackages :: Text -> [PackageMetadata] -> IO [Text]
selectPackages title packages = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  hideCursor
  putStrLn $ T.unpack title
  putStrLn "(Use arrow keys to move, Space to toggle, Enter to confirm)"
  
  selectedStates <- packageLoop 0 (replicate (length packages) False) packages
  
  showCursor
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return [pmName p | (state, p) <- zip selectedStates packages, state]

packageLoop :: Int -> [Bool] -> [PackageMetadata] -> IO [Bool]
packageLoop cursor states packages = do
  -- Draw the list
  forM_ (zip3 [0..] states packages) $ \(idx, state, pkg) -> do
    let prefix = if idx == cursor then "> " else "  "
    let checkbox = if state then "[x] " else "[ ] "
    setSGR [SetColor Foreground Vivid (if idx == cursor then Cyan else White)]
    putStr $ prefix <> checkbox <> T.unpack (T.justifyLeft 25 ' ' (pmName pkg))
    setSGR [SetColor Foreground Dull White]
    putStr $ " - " <> T.unpack (T.take 50 (pmSynopsis pkg))
    clearLine
    putStr "\n"
    setSGR [Reset]
  
  -- Draw Detail pane if needed
  -- setCursorPosition (cursor) 80
  -- putStr "DETAILS..."

  hFlush stdout
  cursorUp (length packages)
  c <- getChar
  case c of
    '\n' -> cursorDown (length packages) >> return states
    ' ' -> let (before, rest) = splitAt cursor states
               newStates = case rest of (s:after) -> before ++ [not s] ++ after; [] -> states
           in packageLoop cursor newStates packages
    '\ESC' -> do
      c2 <- getChar
      if c2 == '[' then do
        c3 <- getChar
        case c3 of
          'A' -> packageLoop (max 0 (cursor - 1)) states packages
          'B' -> packageLoop (min (length packages - 1) (cursor + 1)) states packages
          _ -> packageLoop cursor states packages
      else packageLoop cursor states packages
    _ -> packageLoop cursor states packages

-- | Dashboard to toggle boolean states of items (starts with current states)
toggleDashboard :: Text -> [(Text, Bool)] -> IO [(Text, Bool)]
toggleDashboard title itemsWithStates = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ T.unpack title
  putStrLn "(Use arrow keys to move, Space to toggle, Enter to confirm)"
  let (items, states) = unzip itemsWithStates
  finalStates <- loop 0 states items
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return $ zip items finalStates

loop :: Int -> [Bool] -> [Text] -> IO [Bool]
loop cursor states items = do
  forM_ (zip3 [0..] states items) $ \(idx, state, item) -> do
    let prefix = if idx == cursor then "> " else "  "
    let checkbox = if state then "[x] " else "[ ] "
    setSGR [SetColor Foreground Vivid (if idx == cursor then Cyan else White)]
    putStr $ prefix <> checkbox <> T.unpack item
    clearLine
    putStr "\n"
    setSGR [Reset]
  cursorUp (length items)
  c <- getChar
  case c of
    '\n' -> cursorDown (length items) >> return states
    ' ' -> let (before, rest) = splitAt cursor states
               newStates = case rest of (s:after) -> before ++ [not s] ++ after; [] -> states
           in loop cursor newStates items
    '\ESC' -> do
      c2 <- getChar
      if c2 == '[' then do
        c3 <- getChar
        case c3 of
          'A' -> loop (max 0 (cursor - 1)) states items
          'B' -> loop (min (length items - 1) (cursor + 1)) states items
          _ -> loop cursor states items
      else loop cursor states items
    _ -> loop cursor states items