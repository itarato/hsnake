module Main where

import Control.Concurrent
import Control.Exception qualified as Exception
import Control.Monad
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error qualified as IOError
import System.Posix.IO (stdInput)
import System.Posix.Terminal

data GameState = MakeGameState
  { frame :: (Int, Int),
    direction :: Int,
    parts :: [(Int, Int)],
    dead :: Bool
  }
  deriving (Show)

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

cursorToXY :: Int -> Int -> IO ()
cursorToXY row col = putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"

getTermSize :: IO (Maybe (Int, Int))
getTermSize = do
  window <- TSize.size
  return $ case window of
    Just TSize.Window {TSize.height = h, TSize.width = w} -> Just (w, h)
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

gameLoop :: GameState -> IO ()
gameLoop state = do
  if dead state
    then do
      putStr "Second iteration"
      hFlush stdout
      return ()
    else do
      -- cursorToXY 10 20
      putStr "First iteration"
      hFlush stdout
      threadDelay 1_000_000
      -- cursorToXY 10 20
      -- putStr "XXX"
      -- cursorToXY 11 20
      -- putStr "XXX"
      -- mapM (\n -> putStr (show n) >> hFlush stdout >> guard (n <= 5) >> threadDelay 300_000) [1 .. 10]
      gameLoop state {direction = direction state + 1, dead = True}

-- cursorToXY 10 20
-- putStr "Hello, Haskell!"
-- hFlush stdout
-- threadDelay 3_000_000
-- cursorToXY 10 20
-- putStr "XXX"
-- cursorToXY 11 20
-- putStr "XXX"
-- mapM (\n -> putStr (show n) >> hFlush stdout >> guard (n <= 5) >> threadDelay 300_000) [1 .. 10]
-- return ()

main :: IO ()
main = catchIOException $ do
  oldTerminalConfig <- enableTerminalRawMode
  Exception.finally
    ( do
        clearScreen
        termSize <- getTermSize >>= maybeToIOException "Failed reading terminal size"
        let gameState = MakeGameState termSize 0 [] False
        gameLoop gameState
        return ()
    )
    (restoreTerminalMode oldTerminalConfig)
  where
    catchIOException :: IO () -> IO ()
    catchIOException ioAction = Exception.catch ioAction $ \e -> putStrLn "Error: " >> print @IOError e
