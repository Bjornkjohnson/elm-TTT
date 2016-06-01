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

checkWinner : List (List Cell) -> Cell
checkWinner board =
   List.filter checkListForWin (gatherBoardLists board)
   |> List.head
   |> Maybe.withDefault []
   |> List.head
   |> Maybe.withDefault Empty

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

getBothDiagonals : List (List Cell) -> List (List Cell)
getBothDiagonals board =
 [(getDiagonal board), (getAntiDiagonal board)]

gatherBoardLists : List (List Cell) -> List (List Cell)
gatherBoardLists board =
  List.concat [board, (getAllColumns board), (getBothDiagonals board)]

extractFromList : List a -> Int -> Maybe a
extractFromList list desiredIndex =
  List.head <| List.drop desiredIndex list

setNthItem : List a -> Int  -> a  -> List a
setNthItem list index value=
  Array.fromList list |> set index value |> Array.toList

update : GameState -> Coords -> List (List Cell)
update gameState coords =
  let
    targetRow = getRow gameState.board coords.x
  in
    setNthItem gameState.board coords.x (setNthItem targetRow coords.y gameState.activePlayer)
