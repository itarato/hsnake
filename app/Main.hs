{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Concurrent
import Control.Exception qualified as Exception
import Control.Monad (when)
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hReady, hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error qualified as IOError
import System.Posix.IO (stdInput)
import System.Posix.Terminal

newtype Coord = Coord (Int, Int) deriving (Show, Eq)

instance Semigroup Coord where
  (<>) (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)

instance Monoid Coord where
  mempty = Coord (0, 0)

pattern NORTH :: Int
pattern NORTH = 0

pattern EAST :: Int
pattern EAST = 1

pattern SOUTH :: Int
pattern SOUTH = 2

pattern WEST :: Int
pattern WEST = 3

pattern PART :: Char
pattern PART = '█'

data GameState = MakeGameState
  { frame :: Coord,
    direction :: Int,
    parts :: [Coord],
    dead :: Bool,
    grow :: Int
  }
  deriving (Show)

initGameState :: Coord -> GameState
initGameState frame@(Coord (w, h)) = MakeGameState frame NORTH [Coord (w `div` 2, h `div` 2)] False 3

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

cursorToXY :: Coord -> IO ()
cursorToXY (Coord (col, row)) = putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

getTermSize :: IO (Maybe Coord)
getTermSize = do
  window <- TSize.size
  return $ case window of
    Just TSize.Window {TSize.height = h, TSize.width = w} -> Just (Coord (w, h))
    Nothing -> Nothing

maybeToIOException :: String -> Maybe a -> IO a
maybeToIOException _ (Just a) = return a
maybeToIOException msg _ = Exception.throwIO . IOError.userError $ msg

enableTerminalRawMode :: IO TerminalAttributes
enableTerminalRawMode = do
  oldConfig <- getTerminalAttributes stdInput
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  let newConfig =
        oldConfig
          `withoutMode` EnableEcho
          `withoutMode` ProcessInput
          `withoutMode` KeyboardInterrupts
          `withoutMode` InterruptOnBreak

  setTerminalAttributes stdInput newConfig Immediately
  hideCursor
  return oldConfig

restoreTerminalMode :: TerminalAttributes -> IO ()
restoreTerminalMode oldConfig = do
  setTerminalAttributes stdInput oldConfig Immediately
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  showCursor

putStrAndFlush :: String -> IO ()
putStrAndFlush s = do
  putStr s
  hFlush stdout

directionMap :: Int -> Coord
directionMap NORTH = Coord (0, -1)
directionMap EAST = Coord (1, 0)
directionMap SOUTH = Coord (0, 1)
directionMap WEST = Coord (-1, 0)
directionMap _ = error "Invalid direction"

newHead :: GameState -> Coord
newHead state = directionMap (direction state) <> head (parts state)

nonBlockGetChar :: IO (Maybe Char)
nonBlockGetChar = do
  stdin_ready <- hReady stdin
  if stdin_ready
    then do
      Just <$> getChar
    else do
      return Nothing

newDirection :: Maybe Char -> Int -> Int
newDirection (Just 'w') _ = NORTH
newDirection (Just 'd') _ = EAST
newDirection (Just 's') _ = SOUTH
newDirection (Just 'a') _ = WEST
newDirection _ d = d

didHitExit :: Maybe Char -> Bool
didHitExit (Just '\ESC') = True
didHitExit _ = False

didBiteItself :: Coord -> [Coord] -> Bool
didBiteItself = elem

truncateTail :: [Coord] -> Bool -> [Coord]
truncateTail xs False = xs
truncateTail [] True = []
truncateTail [_] True = []
truncateTail (x : xs) True = x : truncateTail xs True

gameLoop :: GameState -> IO ()
gameLoop state = do
  if dead state
    then do
      putStrAndFlush "Game Over"
    else do
      let newHead' = newHead state
      let needShrink = grow state == 0
      let newParts = newHead' : truncateTail (parts state) needShrink
      let newGrow = if needShrink then grow state else grow state - 1
      cursorToXY newHead'
      putChar PART
      when needShrink $
        do
          cursorToXY $ last (parts state)
          putChar ' '
      hFlush stdout
      input <- nonBlockGetChar
      let newDirection' = newDirection input (direction state)
      let newDead = dead state || didHitExit input || didBiteItself newHead' (parts state)
      threadDelay 100_000
      gameLoop state {parts = newParts, direction = newDirection', dead = newDead, grow = newGrow}

drawBaseState :: GameState -> IO ()
drawBaseState state = do
  mapM_ drawPart (parts state)
  hFlush stdout
  return ()
  where
    drawPart coord = do
      cursorToXY coord
      putChar PART

main :: IO ()
main = catchIOException $ do
  oldTerminalConfig <- enableTerminalRawMode
  Exception.finally
    ( do
        clearScreen
        termSize <- getTermSize >>= maybeToIOException "Failed reading terminal size"
        let gameState = initGameState termSize
        drawBaseState gameState
        gameLoop gameState
        return ()
    )
    (restoreTerminalMode oldTerminalConfig)
  where
    catchIOException :: IO () -> IO ()
    catchIOException ioAction = Exception.catch ioAction $ \e -> putStrLn "Error: " >> print @IOError e
