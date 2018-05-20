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
printDocs title docs = liftIO $ do
    putStrLn title
    x <- mapM (print . exclude ["_id"]) docs
    print $ docs !! 0
    scotty 3000 $
        get "/:word" $ do
            {- beam <- param "word" -}
            json $ L.pack $ show $ docs !! 0


