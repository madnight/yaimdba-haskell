{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib

import Database.MongoDB    (Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)

main :: IO ()
main = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "local" run
    close pipe
    print e

run :: Action IO ()
run = do
    newYorkTeams >>= printDocs "New York Teams"

newYorkTeams :: Action IO [Document]
newYorkTeams = rest =<< find (select ["tconst" =: "tt0701041"] "title.ratings") {sort = ["averageRating" =: 1]}

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs
