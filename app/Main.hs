{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid

-- newtype ServerState = ServerState {notes :: IORef [Note]}

type Server a = SpockM () () () a

app :: Server ()
app = get root $
  lucid $ do
    h1_ "suppp"
    p_ "yooo"

main :: IO ()
main = do
  cfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock cfg app)
