{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib

import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)
import Web.Scotty

import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Char8

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    scotty 3000 $
        get "/:word" $ do
            beam <- param "word"
            e <- liftIO $ access pipe master "local" (episodes beam)
            {- json $ L.pack $ show e -}
            json $ map aesonify e
    close pipe
{- "tt0701041" -}

rating :: String -> Action IO [Document]
rating id =
    rest =<< find (select ["tconst" =: id] "title.ratings")

titleToID :: String -> Action IO [Document]
titleToID id =
    rest =<< find (select ["title" =: id] "title.akas")

episodes :: String -> Action IO [Document]
episodes id =
    rest =<< find (select ["parentTconst" =: id] "title.episode")
