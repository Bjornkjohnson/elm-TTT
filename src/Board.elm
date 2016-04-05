module Board where

import DataTypes exposing (..)
import Array exposing (..)
import Set
newBoard : List (List Cell)
newBoard =
  [ [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  ]

getSymbol : Player -> Cell
getSymbol player =
  case player of
    Human -> X
    Computer -> O

listIsUniform : List Cell -> Bool
listIsUniform list =
  (List.length <| Set.toList <| Set.fromList <| List.map toString list) == 1

empty : List Cell -> Bool
empty list =
  (listIsUniform list) && List.member Empty list

checkListForWin : List Cell -> Bool
checkListForWin list =
  (listIsUniform list) && not (empty list)

checkWinner : List (List Cell) -> String
checkWinner board =
  "hi"

getRow : List (List Cell) -> Int -> List Cell
getRow board row =
  Maybe.withDefault [] <| get row <| fromList (board)

getColumn : List (List Cell) -> Int -> List Cell
getColumn board column =
 List.map (\list -> Maybe.withDefault Empty <| extractFromList list column) board

getAllColumns : List (List Cell) -> List (List Cell)
getAllColumns board =
  List.indexedMap (\idx list -> getColumn board idx) board

getDiagonal : List (List Cell) -> List Cell
getDiagonal board =
 List.indexedMap (\index list -> Maybe.withDefault Empty <| extractFromList list index) board

getAntiDiagonal : List (List Cell) -> List Cell
getAntiDiagonal board =
 getDiagonal <| List.map List.reverse board

extractFromList : List a -> Int -> Maybe a
extractFromList list desiredIndex =
  List.head <| List.drop desiredIndex list

setNthItem : List a -> Int  -> a  -> List a
setNthItem list index value=
  toList <| set index value (fromList list)

update : List (List Cell) -> Int -> Int -> Player -> List (List Cell)
update board row column player =
  setNthItem board row (setNthItem (getRow board row) column (getSymbol player))
