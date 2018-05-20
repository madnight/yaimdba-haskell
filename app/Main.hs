{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib

import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find, valueAt, typed,
                            host, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)
import Web.Scotty

import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as Char8
import Control.Monad (forM)

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    scotty 3000 $
        get "/:word" $ do
            beam <- param "word"
            e <- liftIO $ access pipe master "local" (episodes beam)
            {- json $ L.pack $ show e -}
            let z o = liftIO $ access pipe master "local" (ratings o)
            x <- z ((e))
            {- x <- forM (getIds e) z -}
            {- json $ map aesonify (flatten x) -}
            json $ map aesonify x
            {- text $ getIds e -}
    close pipe
{- "tt0701041" -}

getIds :: [Document] -> [String]
getIds x = map (typed . valueAt "tconst") x


rating :: String -> Action IO [Document]
rating id =
    rest =<< find (select ["$or" =: [["tconst" =: id], ["tconst" =: "tt0096697"]]] "title.ratings")

ratings :: [Document] -> Action IO [Document]
ratings id =
    rest =<< find (select ["$or" =: id] "title.ratings")



titleToID :: String -> Action IO [Document]
titleToID id =
    rest =<< find (select ["title" =: id] "title.akas")

episodes :: String -> Action IO [Document]
episodes id =
    rest =<< find (select ["parentTconst" =: id] "title.episode") {
        {- sort = ["seasonNumber" =: 1, "episodeNumber" =: 1], -}
        project = ["_id" =: 0, "parentTconst" =: 0, "seasonNumber" =: 0, "episodeNumber" =: 0]
        }
