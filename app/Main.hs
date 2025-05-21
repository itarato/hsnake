module Main where

import Control.Concurrent
import Control.Exception qualified as Exception
import System.Console.Terminal.Size qualified as TSize
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.IO.Error qualified as IOError
import System.Posix.IO (stdInput)
import System.Posix.Terminal

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

cursorToXY :: Int -> Int -> IO ()
cursorToXY row col = putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"

termSize :: IO (Maybe (Int, Int))
termSize = do
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

main :: IO ()
main = catchIOException $ do
  oldTerminalConfig <- enableTerminalRawMode
  Exception.finally
    ( do
        clearScreen
        _termSize <- termSize >>= maybeToIOException "Failed reading terminal size"
        cursorToXY 10 20
        putStr $ "Hello, Haskell!" <> show _termSize
        hFlush stdout
        threadDelay 3_000_000
        cursorToXY 10 20
        putStr "XXX"
        cursorToXY 11 20
        putStr "XXX"
        return ()
    )
    (restoreTerminalMode oldTerminalConfig)
  where
    catchIOException :: IO () -> IO ()
    catchIOException ioAction = Exception.catch ioAction $ \e -> putStrLn "Error: " >> print @IOError e
