module Display (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Signal, Address)

import Board exposing (..)
import Ai exposing (..)
import DataTypes exposing (..)
import Mailbox exposing (..)


cellStyle : Attribute
cellStyle =
  style
    [("margin", "10px")
    , ("width", "30%")
    , ("height", "200px")
    , ("font-size", "6em")
    , ("border-radius", "50")
    , ("background", "white")
    , ("border", "2px solid #4A4A4A")]

computerMoveButtonStyle: Attribute
computerMoveButtonStyle =
    style
    [("margin", "10px")
    , ("width", "30%")
    , ("height", "25px")
    , ("font-size", "1em")
    , ("border-radius", "50")
    , ("background", "white")
    , ("border", "2px solid #4A4A4A")]


header : Attribute
header =
  style
    [("text-align", "center")]

convertCellToString : Int -> Cell -> String
convertCellToString index cell =
  case cell of
    Empty -> toString (index + 1)
    _ -> toString cell

buildWinnerString : Cell -> String
buildWinnerString cell =
  case cell of
    Empty -> ""
    _ -> (toString cell) ++ " Wins"

takeTurn: Int -> Action
takeTurn index =
    (Move (index//boardSize)(index % boardSize))

takeComputerTurn: Action
takeComputerTurn =
  (ComputerMove)

createCellButton : Int -> Cell -> Html
createCellButton index cell =
  let
    content = convertCellToString index cell
  in
  button [cellStyle, onClick actions.address (takeTurn index)] [text content]

buildComputerMoveButton: GameState -> Html
buildComputerMoveButton gameState =
  button [computerMoveButtonStyle, onClick actions.address (takeComputerTurn)] [text "Make Computer Move"]

view : Address Action -> GameState -> Html
view address game =
  div [] [
    h1 [header] [ text "Welcome to Tic Tac Toe"],
    (buildComputerMoveButton game),
    div [] (List.indexedMap createCellButton (List.concat game.board)),
    h1 [header] [text (buildWinnerString game.winner)]
  ]
