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
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Text.Internal
import Control.Monad (forM)

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    scotty 3000 $
        get "/:word" $ do
            imdbID <- param "word"
            let fetch = ((liftIO . access pipe master "local") .)
            e <- fetch episodes imdbID
            r <- fetch ratings e
            t <- fetch titles e
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
    rest =<< find (select ["$or" =: include ["tconst"] <$> id] collection) {
            project = ["_id" =: 0]
        }


ratings :: [Document] -> Action IO [Document]
ratings = findSelect "title.ratings"

titles :: [Document] -> Action IO [Document]
titles = findSelect "title.basics"


titleToID :: String -> Action IO [Document]
titleToID id =
    rest =<< find (select ["title" =: id] "title.akas")

episodes :: String -> Action IO [Document]
episodes id =
    rest =<< find (select ["parentTconst" =: id] "title.episode") {
            project = ["_id" =: 0]
        }



        {- sort = ["seasonNumber" =: 1, "episodeNumber" =: 1], -}
