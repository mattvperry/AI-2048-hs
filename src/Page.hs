{-# LANGUAGE OverloadedStrings #-}
module Page
    (
      gameSite
    , jsInject
    , makeMove
    , getScore
    , getGameState
    ) where

import Board
import Test.WebDriver
import Data.List (transpose)
import qualified Data.Aeson as A
import qualified Data.Text as T

gameSite :: WD ()
gameSite = openPage "http://gabrielecirulli.github.io/2048/"

pageBody :: WD Element
pageBody = findElem $ ByTag "body"

getScore :: WD Int
getScore = executeJS [] "return GameManager._instance.score"

makeMove :: Moves -> WD ()
makeMove m = do
    executeJS [] $ T.pack $ "GameManager._instance.move(" ++ (show . fromEnum $ m) ++ ")" :: WD A.Value
    return ()

getGameState :: WD Board
getGameState = do
    board <- executeJS [] "return GameManager._instance.grid.cells.map(function(col) { return col.map(function(tile) { return tile ? tile.value : 0 }) })" :: WD Board
    return . transpose $ board

jsInject :: WD ()
jsInject = do
    elem <- pageBody
    funcTmp <- executeJS [] "return GameManager.prototype.isGameTerminated.toString();" :: WD String
    executeJS [] "GameManager.prototype.isGameTerminated = function() { GameManager._instance = this; return true; }" :: WD A.Value
    sendKeys "s" elem
    executeJS [] $ T.pack $ "eval(GameManager.prototype.isGameTerminated = " ++ funcTmp ++ ")" :: WD A.Value
    return ()
