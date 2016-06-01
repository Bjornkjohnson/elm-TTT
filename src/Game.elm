module Game (..) where

import Html exposing (Html)
import Signal

import Ai exposing (..)
import Board exposing (..)
import DataTypes exposing (..)
import Display exposing (..)
import Mailbox exposing (..)
import Debug

startGame : GameState
startGame =
  {
    board = newBoard,
    activePlayer = X,
    inactivePlayer = O,
    winner = Empty
  }

updateGameBoard : GameState -> Coords -> GameState
updateGameBoard gameState coordSet =
  { gameState | board = Board.update gameState coordSet }

updateWinnerStatus : GameState -> GameState
updateWinnerStatus gameState  =
  { gameState | winner = checkWinner gameState.board }

update : Action -> GameState -> GameState
update action gameState =
  case action of
    NoOp -> gameState
    ComputerMove ->
      let
        nextTurnState = (setPlayerToO gameState)
        nextMove      = (minimaxMove nextTurnState)
      in
        (updateGameBoard nextTurnState (fromFlatIndex nextMove)) |> updateWinnerStatus
    Move row column ->
      let
        nextTurnState = (setPlayerToX gameState)
      in
        updateGameBoard nextTurnState (toCoords row column) |> updateWinnerStatus

toCoords : Int -> Int -> Coords
toCoords row column =
  { x = row, y = column }

setPlayerToX: GameState -> GameState
setPlayerToX lastState =
  {  board = lastState.board,
     activePlayer = X,
     inactivePlayer = O,
     winner = lastState.winner
  }

setPlayerToO: GameState -> GameState
setPlayerToO lastState =
  {  board = lastState.board,
     activePlayer = O,
     inactivePlayer = X,
     winner = lastState.winner
  }

model : Signal GameState
model =
  Signal.foldp update startGame actions.signal

main : Signal Html
main =
  Signal.map (view actions.address) model

