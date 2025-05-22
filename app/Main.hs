{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Concurrent
import Control.Exception qualified as Exception
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error qualified as IOError
import System.Posix.IO (stdInput)
import System.Posix.Terminal

newtype Coord = Coord (Int, Int) deriving (Show)

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

pattern PART :: String
pattern PART = "X"

data GameState = MakeGameState
  { frame :: Coord,
    direction :: Int,
    parts :: [Coord],
    dead :: Bool
  }
  deriving (Show)

initGameState :: Coord -> GameState
initGameState frame@(Coord (w, h)) = MakeGameState frame NORTH [Coord (w `div` 2, h `div` 2)] False

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

cursorToXY :: Coord -> IO ()
cursorToXY (Coord (col, row)) = putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"

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

  return oldConfig

restoreTerminalMode :: TerminalAttributes -> IO ()
restoreTerminalMode oldConfig = do
  setTerminalAttributes stdInput oldConfig Immediately
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True

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

gameLoop :: GameState -> IO ()
gameLoop state = do
  if length (parts state) > 10
    then do
      putStrAndFlush "Game Over"
    else do
      let newHead' = newHead state
      let newParts = newHead' : parts state
      cursorToXY newHead'
      putStr PART
      hFlush stdout
      threadDelay 1_000_000
      gameLoop state {parts = newParts, dead = True}

drawBaseState :: GameState -> IO ()
drawBaseState state = do
  mapM_ drawPart (parts state)
  hFlush stdout
  return ()
  where
    drawPart coord = do
      cursorToXY coord
      putStr PART

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
