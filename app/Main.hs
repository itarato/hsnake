module Main where

import Control.Exception qualified as Exception
import System.Console.Terminal.Size qualified as TSize
import System.IO.Error qualified as IOError

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

main :: IO ()
main = catchIOException $ do
  clearScreen
  _termSize <- termSize >>= maybeToIOException "Failed reading terminal size"
  cursorToXY 10 20
  putStr $ "Hello, Haskell!" <> show _termSize
  cursorToXY 10 20
  putStr "XXX"
  cursorToXY 11 20
  putStr "XXX"
  return ()
  where
    catchIOException :: IO () -> IO ()
    catchIOException ioAction = Exception.catch ioAction $ \e -> putStrLn "Error: " >> print @IOError e
