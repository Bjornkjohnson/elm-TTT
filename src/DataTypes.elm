module DataTypes where

type alias GameState =
  {board : List (List Cell),
   activePlayer : Cell,
   inactivePlayer : Cell,
   winner: Cell }

type alias Coords =
  { x: Int,
    y: Int }

boardSize = 3

type Cell
  = X
  | O
  | Empty

type Player
  = Human
  | Computer

type Action = NoOp | Move Int Int | ComputerMove

