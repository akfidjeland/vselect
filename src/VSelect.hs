{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

import Control.Monad (when)
import Data.IORef
import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stdout, stderr, hGetContents)
import System.Posix.Files (getFileStatus, isCharacterDevice)
import System.Posix.IO
import System.Posix.Types (Fd)

import SelectWidget (selectWidget)


getInput :: Fd -> IO [String]
getInput fd = do
    args <- getArgs
    if null args
        then do
            input <- hGetContents =<< fdToHandle fd
            when (null input) $ exitFailure
            return $! lines input
        else return args


die :: String -> IO ()
die msg = hPutStrLn stderr msg >> exitFailure


{- | Redirect input/output pipes
 -
 - Both input and output may be connected to pipes. This is problematic
 - when running the UI, since this needs input and output from terminal.
 - To deal with duplicate input and output streams for interaction with
 - other shell processes, and set the UI input/output to /dev/tty
 -}
openFiles :: IO (Fd, Fd, Fd)
openFiles = do
    let ttyFile = "/dev/tty"
    ttyStatus <- getFileStatus ttyFile
    when (not $ isCharacterDevice ttyStatus) $ die "Cannot find /dev/tty"
    inputPipe  <- dup stdInput
    outputPipe <- dup stdOutput
    tty <- openFd ttyFile ReadWrite Nothing defaultFileFlags
    dupTo tty stdInput
    dupTo tty stdOutput
    return $! (inputPipe, outputPipe, tty)



main :: IO ()
main = do

    (inputPipe, outputPipe, tty) <- openFiles
    input <- getInput inputPipe

    output <- newIORef ""
    let exit str = writeIORef output str >> shutdownUi

    wList <- selectWidget input exit
    ui <- centered wList

    fg <- newFocusGroup
    addToFocusGroup fg wList

    fg `onKeyPressed` \_ k _ -> do
         case k of
           (KASCII 'q') -> exitFailure
           _ -> return False

    c <- newCollection
    _ <- addToCollection c ui fg

    runUi c defaultContext

    dupTo outputPipe stdOutput
    hPutStrLn stdout =<< readIORef output
    closeFd tty
