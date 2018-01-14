{-# LANGUAGE OverloadedStrings #-}
module Web where

import Web.Scotty

server :: ScottyM ()
server = do
    get "/" $ do
        file "./static/index.html"
    post "/api/limits" $ do
        text "post ok"

runServer :: Int -> IO()
runServer port = scotty port server