{-# LANGUAGE OverloadedStrings #-}
import Test.WebDriver
import Page
import AI
import Control.Monad

myConfig :: WDConfig
myConfig = defaultConfig {
      wdHost            = "192.168.1.2"
    , wdCapabilities    = defaultCaps { browser = chrome }
    }

play :: WD ()
play = do
    gState <- getGameState
    let bestMove = findBestMove gState
    makeMove bestMove
    play

main = runSession myConfig $ do
    gameSite
    jsInject
    play
