{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Concurrent
import Control.Exception qualified as Exception
import Control.Monad (when)
import Data.Time.Clock.POSIX
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hReady, hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error qualified as IOError
import System.Posix.IO (stdInput)
import System.Posix.Terminal
import System.Random

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

pattern FOOD :: Char
pattern FOOD = '$'

pattern GROW_SIZE :: Int
pattern GROW_SIZE = 3

newtype Coord = Coord (Int, Int) deriving (Show, Eq)

instance Semigroup Coord where
  (<>) (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)

instance Monoid Coord where
  mempty = Coord (0, 0)

data GameState = MakeGameState
  { frame :: Coord,
    direction :: Int,
    parts :: [Coord],
    dead :: Bool,
    grow :: Int,
    rng :: StdGen,
    food :: Coord
  }
  deriving (Show)

-- TODO replace RNG seed with time
initGameState :: Coord -> Int -> GameState
initGameState frame@(Coord (w, h)) rngSeed = MakeGameState frame NORTH [Coord (w `div` 2, h `div` 2)] False 3 rng' foodCoord
  where
    rng = mkStdGen rngSeed
    (foodCoord, rng') = randCoord rng frame

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

nonBlockGetChar :: Maybe Char -> IO (Maybe Char)
nonBlockGetChar prev_input = do
  stdin_ready <- hReady stdin
  if stdin_ready
    then do
      c <- getChar
      -- Makes sure it flushes the stdin buffer and keeps only the last one
      nonBlockGetChar (Just c)
    else do
      return prev_input

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

-- TODO Add a wrapper that calls this until it does not match a provided list of coords (no new coord on snake).
randCoord :: (RandomGen r) => r -> Coord -> (Coord, r)
randCoord rng (Coord (w, h)) = (coord, rng'')
  where
    (w', rng') = randomR (1, w - 1) rng
    (h', rng'') = randomR (1, h - 1) rng'
    coord = Coord (w', h')

gameLoop :: GameState -> IO ()
gameLoop state = do
  if dead state
    then do
      -- TODO Shrinking dying animation.
      putStrAndFlush "Game Over"
    else do
      let newHead' = newHead state
      let needShrink = grow state == 0
      let newParts = newHead' : truncateTail (parts state) needShrink
      let newGrow = if needShrink then grow state else grow state - 1
      let didEatFood = newHead' == food state
      let (newFood, newRng) = if didEatFood then randCoord (rng state) (frame state) else (food state, rng state)
      let newGrow' = if didEatFood then newGrow + GROW_SIZE else newGrow
      cursorToXY newHead'
      putChar PART
      when needShrink $
        do
          cursorToXY $ last (parts state)
          putChar ' '
      when didEatFood $
        do
          cursorToXY newFood
          putChar FOOD
      hFlush stdout
      input <- nonBlockGetChar Nothing
      let newDirection' = newDirection input (direction state)
      -- TODO Die also when reaching frame.
      let newDead = dead state || didHitExit input || didBiteItself newHead' (parts state)
      threadDelay 100_000
      gameLoop state {parts = newParts, direction = newDirection', dead = newDead, grow = newGrow', food = newFood, rng = newRng}

drawBaseState :: GameState -> IO ()
drawBaseState state = do
  mapM_ drawPart (parts state)
  cursorToXY $ food state
  putChar FOOD
  hFlush stdout
  return ()
  where
    drawPart coord = do
      cursorToXY coord
      putChar PART

getCurrentTimestamp :: IO Int
getCurrentTimestamp = floor <$> getPOSIXTime

main :: IO ()
main = catchIOException $ do
  oldTerminalConfig <- enableTerminalRawMode
  Exception.finally
    ( do
        clearScreen
        termSize <- getTermSize >>= maybeToIOException "Failed reading terminal size"
        rngSeed <- getCurrentTimestamp
        let gameState = initGameState termSize rngSeed
        drawBaseState gameState
        gameLoop gameState
        return ()
    )
    (restoreTerminalMode oldTerminalConfig)
  where
    catchIOException :: IO () -> IO ()
    catchIOException ioAction = Exception.catch ioAction $ \e -> putStrLn "Error: " >> print @IOError e
