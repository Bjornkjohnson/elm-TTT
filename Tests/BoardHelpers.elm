module BoardHelpers (..) where
import DataTypes exposing (..)

getOActiveStateFor : List (List Cell) -> GameState
getOActiveStateFor board =
  { board = board,
    activePlayer = O,
    inactivePlayer = X,
    winner = Empty }

getXActiveStateFor : List (List Cell) -> GameState
getXActiveStateFor board =
  { board = board,
    activePlayer = X,
    inactivePlayer = O,
    winner = Empty }

empty3x3Board : List (List Cell)
empty3x3Board =
  [ [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  , [Empty, Empty, Empty]
  ]

test3x3Board : List (List Cell)
test3x3Board =
  [ [Empty, X,     X]
  , [Empty, O, Empty]
  , [Empty, X, Empty]
  ]

winner3x3Board : List (List Cell)
winner3x3Board =
  [ [    X, X, X]
  , [Empty, O, O]
  , [Empty, X, O]
  ]

oWins3x3Board : List (List Cell)
oWins3x3Board =
  [ [    X, X, O]
  , [Empty, O, O]
  , [Empty, X, O]
  ]

winner3x3BoardColumns : List (List Cell)
winner3x3BoardColumns =
  [ [X, Empty, Empty]
  , [X,     O,    X]
  , [X,     O,    O]
  ]

winner3x3BoardDiagonals : List (List Cell)
winner3x3BoardDiagonals =
  [ [X, O, O]
  , [X, O, Empty]
  ]

all3x3Lists : List (List Cell)
all3x3Lists =
  [ [X,     X,     X]
  , [Empty, O,     O]
  , [Empty, X,     O]
  , [X,     Empty, Empty]
  , [X,     O,     X]
  , [X,     O,     O]
  , [X,     O,     O]
  , [X,     O,     Empty]
  ]

empty4x4Board : List (List Cell)
empty4x4Board =
  [ [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty]
  ]

test4x4Board : List (List Cell)
test4x4Board =
  [ [Empty, X, Empty, Empty]
  , [Empty, O, Empty, Empty]
  , [Empty, X, Empty, Empty]
  , [Empty, O, Empty, Empty]
  ]

winner4x4Board : List (List Cell)
winner4x4Board =
  [ [X, X, Empty, Empty]
  , [O, X, Empty, Empty]
  , [O, X,     X, Empty]
  , [O, O,     O,     X]
  ]

winner4x4BoardColumn : List (List Cell)
winner4x4BoardColumn =
  [ [    X,     O,     O, O]
  , [    X,     X,     X, O]
  , [Empty, Empty,     X, O]
  , [Empty, Empty, Empty, X]
  ]

winner4x4BoardDiagonals : List (List Cell)
winner4x4BoardDiagonals =
  [ [    X,     X, X, X]
  , [Empty, Empty, X, O]
  ]

all4x4Lists : List (List Cell)
all4x4Lists =
  [ [    X,     X, Empty, Empty]
  , [    O,     X, Empty, Empty]
  , [    O,     X,     X, Empty]
  , [    O,     O,     O, X]
  , [    X,     O,     O, O]
  , [    X,     X,     X, O]
  , [Empty, Empty,     X, O]
  , [Empty, Empty, Empty, X]
  , [    X,     X,     X, X]
  , [Empty, Empty,     X, O]
  ]

nearWin : List (List Cell)
nearWin =
  [ [Empty,     X,     O]
  , [    X, Empty,     O]
  , [Empty, Empty, Empty]
  ]

oneEmptySpace : List (List Cell)
oneEmptySpace =
  [ [O, X,     O]
  , [X, X,     O]
  , [O, O, Empty]
  ]
