{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception qualified as Exception
import Control.Monad (forM_, when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hReady, hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error qualified as IOError
import System.Posix.IO (stdInput)
import System.Posix.Terminal
  ( TerminalAttributes,
    TerminalMode
      ( EnableEcho,
        InterruptOnBreak,
        KeyboardInterrupts,
        ProcessInput
      ),
    TerminalState (Immediately),
    getTerminalAttributes,
    setTerminalAttributes,
    withoutMode,
  )
import System.Random
  ( Random (randomR),
    RandomGen,
    StdGen,
    mkStdGen,
  )

pattern NORTH :: Int
pattern NORTH = 0

pattern EAST :: Int
pattern EAST = 1

pattern SOUTH :: Int
pattern SOUTH = 2

pattern WEST :: Int
pattern WEST = 3

pattern PART :: String
pattern PART = "\ESC[94m▒\ESC[0m"

pattern PART_DEAD :: String
pattern PART_DEAD = "\ESC[91m▒\ESC[0m"

pattern HEAD :: String
pattern HEAD = "\ESC[94m█\ESC[0m"

pattern FOOD :: String
pattern FOOD = "\ESC[93m*\ESC[0m"

pattern GROW_SIZE :: Int
pattern GROW_SIZE = 3

pattern FOOD_TRY_LIMIT :: Int
pattern FOOD_TRY_LIMIT = 16

pattern FRAME :: String
pattern FRAME = "\ESC[32m░\ESC[0m"

pattern MIN_SPEED :: Int
pattern MIN_SPEED = 100_000

pattern MAX_SPEED :: Int
pattern MAX_SPEED = 10_000

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
    food :: Coord,
    speed :: Int,
    score :: Int
  }
  deriving (Show)

data KeyStroke = KeyEsc | KeyLeft | KeyRight | KeyUp | KeyDown | KeySpace deriving (Eq)

initGameState :: Coord -> Int -> GameState
initGameState frame@(Coord (w, h)) rngSeed = MakeGameState frame NORTH [Coord (w `div` 2, h `div` 2)] False (GROW_SIZE - 1) rng' foodCoord MIN_SPEED 0
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
newHead state = wrapHead newHead' $ frame state
  where
    newHead' = directionMap (direction state) <> head (parts state)

wrapHead :: Coord -> Coord -> Coord
wrapHead (Coord (x, y)) (Coord (w, h)) = Coord (wrap x 2 (w - 1), wrap y 2 (h - 1))

wrap :: Int -> Int -> Int -> Int
wrap v minV maxV
  | v < minV = maxV
  | v > maxV = minV
  | otherwise = v

nonBlockGetKeyStroke :: [Char] -> IO (Maybe KeyStroke)
nonBlockGetKeyStroke xs
  | length xs > 3 = return Nothing
  | xs == ['\ESC', '[', 'A'] = return $ Just KeyUp
  | xs == ['\ESC', '[', 'B'] = return $ Just KeyDown
  | xs == ['\ESC', '[', 'C'] = return $ Just KeyRight
  | xs == ['\ESC', '[', 'D'] = return $ Just KeyLeft
  | xs == [' '] = return $ Just KeySpace
  | xs == ['\ESC'] = do
      stdin_ready <- hReady stdin
      if stdin_ready
        then do
          c <- getChar
          nonBlockGetKeyStroke ['\ESC', c]
        else return $ Just KeyEsc
  | otherwise = do
      stdin_ready <- hReady stdin
      if stdin_ready
        then do
          c <- getChar
          nonBlockGetKeyStroke $ xs ++ [c]
        else return Nothing

nonBlockGetKeyStrokeAndFlushStdin :: IO (Maybe KeyStroke)
nonBlockGetKeyStrokeAndFlushStdin = do
  result <- nonBlockGetKeyStroke []
  flushStdin
  return result
  where
    flushStdin = do
      stdin_ready <- hReady stdin
      when stdin_ready $ do
        _ <- getChar
        flushStdin

newDirection :: Maybe KeyStroke -> Int -> Int
newDirection (Just KeyDown) NORTH = NORTH
newDirection (Just KeyLeft) EAST = EAST
newDirection (Just KeyRight) WEST = WEST
newDirection (Just KeyUp) SOUTH = SOUTH
newDirection (Just KeyDown) _ = SOUTH
newDirection (Just KeyLeft) _ = WEST
newDirection (Just KeyRight) _ = EAST
newDirection (Just KeyUp) _ = NORTH
newDirection _ d = d

didHitExit :: Maybe KeyStroke -> Bool
didHitExit (Just KeyEsc) = True
didHitExit _ = False

didBiteItself :: Coord -> [Coord] -> Bool
didBiteItself = elem

truncateTail :: [Coord] -> Bool -> [Coord]
truncateTail xs False = xs
truncateTail [] True = []
truncateTail [_] True = []
truncateTail (x : xs) True = x : truncateTail xs True

randCoord :: (RandomGen r) => r -> Coord -> (Coord, r)
randCoord rng (Coord (w, h)) = (coord, rng'')
  where
    (w', rng') = randomR (2, w - 1) rng
    (h', rng'') = randomR (2, h - 1) rng'
    coord = Coord (w', h')

randCoordExcept :: (RandomGen r) => r -> Coord -> [Coord] -> Int -> (Coord, r)
-- TODO This is not elegant. Find something reliable and use Maybe Coord for result.
randCoordExcept _ _ _ 0 = error "Cannot find empty position for food"
randCoordExcept rng frame excepts limit =
  if coord `elem` excepts
    then
      randCoordExcept rng' frame excepts (limit - 1)
    else
      (coord, rng')
  where
    (coord, rng') = randCoord rng frame

inFrame :: Coord -> Coord -> Bool
inFrame (Coord (x, y)) (Coord (w, h))
  | x <= 1 = False
  | y <= 1 = False
  | x >= w = False
  | y >= h = False
  | otherwise = True

newSpeed :: Int -> Maybe KeyStroke -> Int
-- Speed up.
newSpeed s (Just KeySpace) = let s' = round (fromIntegral s * 0.7) in max MAX_SPEED s'
-- Slow down.
newSpeed s _ = let s' = round (fromIntegral s * 1.15) in min MIN_SPEED s'

gameLoop :: GameState -> IO ()
gameLoop state = do
  if dead state
    then gameLoopStageEnd state
    else do
      let newHead' = newHead state
      let needShrink = grow state == 0
      let newParts = newHead' : truncateTail (parts state) needShrink
      let newGrow = if needShrink then grow state else grow state - 1
      let didEatFood = newHead' == food state
      let newScore = if didEatFood then score state + 1 else score state
      let occupiedCoords = food state : newParts
      let (newFood, newRng) = if didEatFood then randCoordExcept (rng state) (frame state) occupiedCoords FOOD_TRY_LIMIT else (food state, rng state)
      let newGrow' = if didEatFood then newGrow + GROW_SIZE else newGrow
      cursorToXY $ head (parts state)
      putStr PART
      cursorToXY newHead'
      putStr HEAD
      when needShrink $
        do
          cursorToXY $ last (parts state)
          putChar ' '
      when didEatFood $
        do
          cursorToXY newFood
          putStr FOOD
      hFlush stdout
      input <- nonBlockGetKeyStrokeAndFlushStdin
      let newDirection' = newDirection input (direction state)
      let didCrossFrame = not $ inFrame newHead' (frame state)
      let newDead = dead state || didHitExit input || didBiteItself newHead' (parts state) || didCrossFrame
      let newSpeed' = newSpeed (speed state) input
      threadDelay newSpeed'
      when didEatFood $ do
        cursorToXY (Coord (3, h))
        putStr $ " Score: " ++ show newScore ++ " "
      gameLoop state {parts = newParts, direction = newDirection', dead = newDead, grow = newGrow', food = newFood, rng = newRng, speed = newSpeed', score = newScore}
  where
    (Coord (_, h)) = frame state

-- TODO Make animation fix time independently from length.
gameLoopStageEnd :: GameState -> IO ()
gameLoopStageEnd state = do
  let animDelay = 2_000_000 `div` length (parts state)
  forM_ (reverse $ parts state) (overPaintPart PART_DEAD 0)
  forM_ (reverse $ parts state) (overPaintPart " " animDelay)
  clearScreen
  cursorToXY $ Coord (1, 1)
  where
    overPaintPart c delay coord = do
      cursorToXY coord
      putStr c
      hFlush stdout
      threadDelay delay

joinStr :: [String] -> String
joinStr = foldl (<>) ""

drawBaseState :: GameState -> IO ()
drawBaseState state = do
  drawFrame
  mapM_ drawPart (parts state)
  cursorToXY $ food state
  putStr FOOD
  hFlush stdout
  return ()
  where
    (Coord (width, height)) = frame state
    drawPart coord = do
      cursorToXY coord
      putStr PART
    hline = FRAME ++ replicate (width - 2) ' ' ++ FRAME
    drawHLine i = do
      cursorToXY $ Coord (1, i)
      putStr hline
    drawFrame = do
      forM_ [2 .. (height - 1)] drawHLine
      cursorToXY $ Coord (1, 1)
      putStr $ joinStr (replicate width FRAME)
      cursorToXY $ Coord (1, height)
      putStr $ joinStr (replicate width FRAME)
      cursorToXY $ Coord (3, 1)
      putStr " hSnake "
      cursorToXY (Coord (3, height))
      putStr " Score: - "

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
