{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

-- TODO: change to single list of (Maybe Int, String)
-- TODO: add markers
-- TODO: make item selection be relative to the current visible top
-- TODO: add on-line search

import Control.Monad (when)
import Data.Char (isDigit, digitToInt)
import Data.IORef
import Data.Maybe ()
import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit (exitFailure)


listWidget input exit = do
    let attr = white `on` green
    w <- newStringList attr input
    setFocusAttribute w $ attr

    w `onItemActivated` \(ActivateItemEvent _ str _) -> exit str

    w `onKeyPressed` \_ k _ -> do
        case k of
            (KASCII 'j') -> scrollDown w >> return True
            (KASCII 'k') -> scrollUp   w >> return True
            (KASCII c) -> do
                when (isDigit c) $ do
                    i <- getListItem w $ (digitToInt c) - 1
                    maybe (return ()) (exit . fst) i
                return False
            _ -> return False
    return w


indexWidget input = newStringList def_attr indices
    where
        indices = take (length input) $ map (:[]) ['1'..'9']


main :: IO ()
main = do

    input <- (return . lines) =<< getContents
    when (null input) exitFailure

    output <- newIORef ""
    let exit str = writeIORef output str >> shutdownUi

    wList <- listWidget input exit
    wIndex <- indexWidget input
    ui <- (hFixed 2 wIndex) <++> (return wList)

    fg <- newFocusGroup
    addToFocusGroup fg wList

    fg `onKeyPressed` \_ k _ -> do
         case k of
           (KASCII 'q') -> exitFailure
           _ -> return False

    c <- newCollection
    addToCollection c ui fg

    runUi c defaultContext
    putStrLn =<< readIORef output
