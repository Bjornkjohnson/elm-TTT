module Ai (..) where

import DataTypes exposing (..)
import Board exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import List exposing (..)
import Array exposing (..)
import Debug exposing (..)

computerMarker : Cell
computerMarker = DataTypes.O

opponentMarker : Cell
opponentMarker = DataTypes.X

getScore : List (List Cell) -> number
getScore board =
  if (checkWinner board == computerMarker)
    then 10
  else if (checkWinner board == opponentMarker)
    then -10
  else 0

nextState: GameState -> Coords -> GameState
nextState gameState move =
  { board = Board.update gameState move,
    activePlayer = gameState.inactivePlayer,
    inactivePlayer = gameState.activePlayer,
    winner = Empty }

isATie: GameState -> Bool
isATie state =
  Array.isEmpty <| Array.fromList <| getEmptySpacesInBoard state.board

theresAWinner: GameState -> Bool
theresAWinner state =
  (Board.checkWinner state.board) /= Empty

gameOver : GameState -> Bool
gameOver state =
  (isATie state) || (theresAWinner state)

getDepthValue : Int -> Cell -> Int
getDepthValue depth activePlayer =
  if (activePlayer == computerMarker) then
    depth * -1
  else
    depth

minimaxScore : GameState -> Int -> Int
minimaxScore gameState depth =
  if (gameOver gameState) then
    getScore gameState.board
  else
    let
      currentAvailableMoves = Array.fromList <| getEmptySpacesInBoard gameState.board
      nextStates            = (getNextRoundOfPossibleGameStates gameState currentAvailableMoves)
      scores                = Array.map (\possibleState -> (minimaxScore possibleState (depth + 1))
                                                            + (getDepthValue depth gameState.activePlayer)) nextStates
    in
      (getMinOrMaxValue scores gameState.activePlayer)

getMinOrMaxValue : Array (Int) -> Cell -> Int
getMinOrMaxValue scores activePlayer =
  if (activePlayer == computerMarker) then (getMaxValue scores) else (getMinValue scores)

scoreEachOpenPosition : GameState -> Array (Coords) -> Array (Int)
scoreEachOpenPosition gameState availableMoves =
  Array.map (\possibleState -> minimaxScore possibleState 0) (getNextRoundOfPossibleGameStates gameState availableMoves)

getNextRoundOfPossibleGameStates : GameState -> Array (Coords) -> Array (GameState)
getNextRoundOfPossibleGameStates gameState availableMoves =
  Array.map (\move -> nextState gameState move) availableMoves

minimaxMove : GameState -> Int
minimaxMove gameState =
  if (gameOver gameState) then
    getScore gameState.board
  else
    let
      availableMoves = Array.fromList <| getEmptySpacesInBoard gameState.board
      scores         = (scoreEachOpenPosition gameState availableMoves)
      chosenMove     = Array.get (getMinOrMaxIndex gameState scores) availableMoves
    in
      toFlatIndex (fromJust chosenMove)

getIndexOf : a -> Array a -> Int
getIndexOf elem coll =
  let
    indexedCollection = List.indexedMap (,) (Array.toList coll)
    searchResults = List.filter (\pair -> (snd pair) == elem) indexedCollection
    unwrappedResult = fromJust <| List.head searchResults
  in
    fst unwrappedResult

getMinOrMaxIndex : GameState -> Array Int -> Int
getMinOrMaxIndex gameState scores =
  if gameState.activePlayer == computerMarker then
    getIndexOf (scores |> getMaxValue) scores
  else
    getIndexOf (scores |> getMinValue) scores

getMaxValue : Array comparable -> Int
getMaxValue scores =
    fromJust <| List.maximum <| Array.toList scores

getMinValue : Array comparable -> Int
getMinValue scores =
    fromJust <| List.minimum <| Array.toList scores

getEmptySpacesInBoard board =
    concat (List.map (\row -> getEmptySpacesInRow(row)) (indexedElements board))

getEmptySpacesInRow rowTuple =
  let
    xVal = fst rowTuple
    emptySpaces = indexedElements(snd rowTuple) |> List.filter (\elem -> (snd elem == Empty))
  in
    List.map (\indexedPair -> {x=xVal, y=(fst indexedPair)}) emptySpaces

indexedElements : List a -> List (Int, a)
indexedElements inputList =
  List.indexedMap (,) inputList

fromJust : Maybe a -> a
fromJust x =
  case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

toCoordSet : (Int, Int) -> Coords
toCoordSet tup =
  {x=(fst tup), y=(snd tup)}

toFlatIndex : Coords -> Int
toFlatIndex coord =
  if coord.x == 0 then
    coord.y
  else if coord.x == 1 then
    coord.y + coord.x + 2
  else if coord.x == 2 then
    coord.x + coord.y + 4
  else
    Debug.crash "error: Out of bounds index"

fromFlatIndex : Int -> Coords
fromFlatIndex index =
  let
    xVal = (calculateX index)
    yVal = (calculateY index)
  in
    {x = xVal, y = yVal}

calculateX: Int -> Int
calculateX index =
  if index <= 2 then
    0
  else if index <= 5 then
    1
  else if index <= 8 then
    2
  else
    Debug.crash "error: Int out of bounds"

calculateY : Int -> Int
calculateY index =
  index % 3
