{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind #-}
module Main where

-- TODO: join index-key to the entry index
-- TODO: add on-line search

import Control.Monad (when, forM, forM_)
import Data.Char (isDigit, digitToInt)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Graphics.Vty
import Graphics.Vty.Widgets.All
import System.Exit (exitFailure)


data Entry = Entry {
        index :: Maybe Char,
        marked :: Bool,
        value :: String
    }


instance Show Entry where
    show (Entry n m ss) = (maybe ' ' id n) : " " ++ marker ++ ss
        where
            marker = if m then "* " else "  "

toggle :: Entry -> Entry
toggle (Entry i m v) = Entry i (not m) v


type ListWidget = (Widget (List Entry FormattedText))


toggleSelected :: ListWidget -> IO ()
toggleSelected w = do
    s <- getSelected w
    case s of
        Nothing -> return ()
        (Just (i, (e, _))) -> do
            removeFromList w i
            let e' = toggle e
            we <- plainText (show e')
            insertIntoList w e' we i
            setSelected w i


allEntries :: ListWidget -> IO [Entry]
allEntries w = do
    n <- getListSize w
    forM [0..n-1] $ \i -> return . fst . fromJust =<< getListItem w i


{- The return is either
 - 1) just the currently selected item (if nothing has been marked); or
 - 2) all marked items
 -}
selected :: ListWidget -> Entry -> IO String
selected w e = do
    entries <- allEntries w
    let markedEntries = filter marked entries
    if null markedEntries
        then return $ value e
        else return $ intercalate "\n" $ map value markedEntries


listWidget :: [String] -> (String -> IO ()) -> IO ListWidget
listWidget input exit = do
    let attr = white `on` green
        indices = (map Just ['1'..'9']) ++ repeat Nothing
        entries = zipWith3 Entry indices (repeat False) input
    w <- newList attr
    forM_ entries $ \e ->
        addToList w e =<< plainText (show e)

    setFocusAttribute w $ attr

    w `onItemActivated` \(ActivateItemEvent _ entry _) -> exit =<< selected w entry

    w `onKeyPressed` \_ k _ -> do
        case k of
            (KASCII 'j') -> scrollDown w     >> return True
            (KASCII 'k') -> scrollUp w       >> return True
            (KASCII 'x') -> toggleSelected w >> return True
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
