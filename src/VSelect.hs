{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

-- TODO: join index-key to the entry index
-- TODO: enforce single-digit keys
-- TODO: add markers
-- TODO: make item selection be relative to the current visible top
-- TODO: add on-line search

import Control.Monad (when, forM_)
import Data.Char (isDigit, digitToInt)
import Data.IORef
import Data.Maybe ()
import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit (exitFailure)


data Entry = Entry {
        index :: Maybe Int,
        value :: String
    }


instance Show Entry where
    show (Entry n ss) = (maybe " " show n) ++ " " ++ ss



listWidget :: [String] -> (String -> IO ()) -> IO (Widget (List Entry FormattedText))
listWidget input exit = do
    let attr = white `on` green
        indices = (map Just [1..9]) ++ repeat Nothing
        entries = zipWith Entry indices input
    w <- newList attr
    forM_ entries $ \e ->
        addToList w e =<< plainText (show e)

    setFocusAttribute w $ attr

    w `onItemActivated` \(ActivateItemEvent _ entry _) -> exit $ value entry

    w `onKeyPressed` \_ k _ -> do
        case k of
            (KASCII 'j') -> scrollDown w >> return True
            (KASCII 'k') -> scrollUp   w >> return True
            (KASCII c) -> do
                when (isDigit c) $ do
                    i <- getListItem w $ (digitToInt c) - 1
                    maybe (return ()) (exit . value . fst) i
                return False
            _ -> return False
    return w


main :: IO ()
main = do

    input <- (return . lines) =<< getContents
    when (null input) exitFailure

    output <- newIORef ""
    let exit str = writeIORef output str >> shutdownUi

    wList <- listWidget input exit
    ui <- centered wList

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
