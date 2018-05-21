{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib
import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find, valueAt,
                            typed, host, insertMany, master, project, rest,
                            select, include, merge, sort, (=:))
import Control.Monad.Trans (liftIO)
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Internal

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    let fetch = ((liftIO . access pipe master "local") .)
    scotty 3000 $
        get "/:word" $ do
            imdbID <- param "word"
            e <- fetch episodes imdbID
            r <- fetch (findSelect "title.ratings") e
            t <- fetch (findSelect "title.basics") e
            json $ aesonify <$> foldr combine e [r, t]
    close pipe
    where
        combine x = ((`mergeByID` x) =<<)


mergeByID :: Document -> [Document] -> [Document]
mergeByID y [] = []
mergeByID y (x:xs)
    | valueAt "tconst" y == valueAt "tconst" x =
        merge y x : mergeByID y xs
    | otherwise = mergeByID y xs


findSelect :: Text -> [Document] -> Action IO [Document]
findSelect collection id =
    rest =<< find (
        select ["$or" =: include ["tconst"] <$> id] collection) {
            project = ["_id" =: 0]
        }

titleToID :: String -> Action IO [Document]
titleToID id =
    rest =<< find (select ["title" =: id] "title.akas")

episodes :: String -> Action IO [Document]
episodes id =
    rest =<< find (select ["parentTconst" =: id] "title.episode") {
            project = ["_id" =: 0]
        }

{- sort = ["seasonNumber" =: 1, "episodeNumber" =: 1], -}
