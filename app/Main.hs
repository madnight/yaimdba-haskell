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
    pipe <- connect (host "mongo")
    let fetch = ((liftIO . access pipe master "local") .)
    scotty 3000 $ do

        get "/getID/:word" $ do
            title <- param "word"
            i <- fetch titleToID title
            r <- fetch (find' "title.ratings") i
            json $ aesonify <$> merge' i r

        get "/:word" $ do
            imdbID <- param "word"
            e <- fetch episodes imdbID
            r <- fetch (find' "title.ratings") e
            t <- fetch (find' "title.basics") e
            json $ aesonify <$> foldr merge' e [r, t]

    close pipe

merge' :: [Document] -> [Document] -> [Document]
merge' = (=<<) . merge''
    where merge'' [] y = []
          merge'' (x:xs) y
            | valueAt "tconst" y == valueAt "tconst" x =
                merge x y : merge'' xs y
            | otherwise = merge'' xs y


find' :: Text -> [Document] -> Action IO [Document]
find' collection id =
    rest =<< find (
        select ["$or" =: include ["tconst"] <$> id] collection) {
            project = ["_id" =: 0]
        }

titleToID :: String -> Action IO [Document]
titleToID id =
    rest =<< find (select ["primaryTitle" =: id] "title.basics")

episodes :: String -> Action IO [Document]
episodes id =
    rest =<< find (select ["parentTconst" =: id] "title.episode") {
            project = ["_id" =: 0]
        }

{- sort = ["seasonNumber" =: 1, "episodeNumber" =: 1], -}
