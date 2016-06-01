module TestAi (..) where

import ElmTest exposing (..)
import Ai exposing (..)
import DataTypes exposing (..)
import BoardHelpers exposing (..)
import Dict exposing (..)
import Array exposing (..)

aiInfrastructureTest : Test
aiInfrastructureTest =
  suite
  "Test Ai Infrastructure"
  [ test
      "getScore returns 10 for current marker win"
      (assertEqual 10 (getScore oWins3x3Board))

  , test
    "getScore returns -10 for opponent marker win"
    (assertEqual -10 (getScore winner3x3Board))

  , test
    "getScore returns 0 for non-winning board"
    (assertEqual 0 (getScore test3x3Board))

  , test
    "selects max score if currentMarker is computer marker"
    (assertEqual 2
      (getMinOrMaxIndex (getOActiveStateFor middleBlockBoard) (Array.fromList [-10, 1, 200])))

  , test
    "selects min score cell if currentMarker is opponent marker"
    (assertEqual 0
      (getMinOrMaxIndex (getXActiveStateFor middleBlockBoard) (Array.fromList [-10, 1, 200])))

  , test
    "getMaxValue gets max"
    (assertEqual 10
      (getMaxValue (Array.fromList [0, 5, 10])))

  , test
    "getMinValue gets max"
    (assertEqual 0
      (getMinValue (Array.fromList [0, 5, 10])))

  , test
    "emptySpaces returns empty spaces - nearly full board"
    (assertEqual [{x=2, y=2}]
      (getEmptySpacesInBoard oneEmptySpace))

  , test
    "emptySpaces returns empty spaces - nearly empty board"
    (assertEqual [{x=0, y=0},
                              {x=1, y=1},
                  {x=2, y=0}, {x=2, y=1}, {x=2, y=2}]
      (getEmptySpacesInBoard(nearWin)))

  , test
    "emptySpacesRow returns empty spaces"
    (assertEqual [{x=0, y=1}, {x=0, y=2}]
      (getEmptySpacesInRow((0, [X, Empty, Empty]))))

  , test
    "fromJust converts numbers"
    (assertEqual 0
      (fromJust (Just 0)))

  , test
    "fromJust converts tuples"
    (assertEqual (0,0)
      (fromJust (Just (0,0))))

  , test
    "converts tuple to coordset"
    (assertEqual {x=1, y=2}
      (toCoordSet (1,2)))

  , test
    "get index returns middle index"
    (assertEqual 1
      (getIndexOf 100 (Array.fromList([10, 100, 0]))))

  , test
    "get index returns first index if multi matches"
    (assertEqual 0
      (getIndexOf 100 (Array.fromList([100, 100, 0]))))

  , test
    "translates 0th coord to position"
    (assertEqual (0//1)
      (toFlatIndex {x=0, y=0}))

  , test
    "translates 1st coord to position"
    (assertEqual (1//1)
      (toFlatIndex {x=0, y=1}))

  , test
    "translates 2nd coord to position"
    (assertEqual (2//1)
      (toFlatIndex {x=0, y=2}))

  , test
    "translates 3rd coord to position"
    (assertEqual (3//1)
      (toFlatIndex {x=1, y=0}))

  , test
    "translates 4th coord to position"
    (assertEqual (4//1)
      (toFlatIndex {x=1, y=1}))

  , test
    "translates 5th coord to position"
    (assertEqual (5//1)
      (toFlatIndex {x=1, y=2}))

  , test
    "translates 6th coord to position"
    (assertEqual (6//1)
      (toFlatIndex {x=2, y=0}))

  , test
    "translates 7th coord to position"
    (assertEqual (7//1)
      (toFlatIndex {x=2, y=1}))

  , test
    "translates 8th coord to position"
    (assertEqual (8//1)
      (toFlatIndex {x=2, y=2}))

  , test
    "translates from flat index 8 to coords"
    (assertEqual {x=2, y=2}
      (fromFlatIndex (8//1)))

  , test
    "translates from flat index 7 to coords"
    (assertEqual {x=2, y=1}
      (fromFlatIndex (7//1)))

  , test
    "translates from flat index 6 to coords"
    (assertEqual {x=2, y=0}
      (fromFlatIndex (6//1)))

  , test
    "translates from flat index 5 to coords"
    (assertEqual {x=1, y=2}
      (fromFlatIndex (5//1)))

  , test
    "translates from flat index 4 to coords"
    (assertEqual {x=1, y=1}
      (fromFlatIndex (4//1)))

  , test
    "translates from flat index 3 to coords"
    (assertEqual {x=1, y=0}
      (fromFlatIndex (3//1)))

  , test
    "translates from flat index 2 to coords"
    (assertEqual {x=0, y=2}
      (fromFlatIndex (2//1)))

  , test
    "translates from flat index 1 to coords"
    (assertEqual {x=0, y=1}
      (fromFlatIndex (1//1)))

  , test
    "translates from flat index 0 to coords"
    (assertEqual {x=0, y=0}
      (fromFlatIndex (0//1)))

  , test
    "game over true if cats game"
    (assertEqual True
      (gameOver (getOActiveStateFor catBoard)))

  , test
    "game over true if self has won"
    (assertEqual True
      (gameOver (getOActiveStateFor oWins3x3Board)))

  , test
    "game over true if opponent has won"
    (assertEqual True
      (gameOver (getXActiveStateFor oWins3x3Board)))

  , test
    "game over false for unfinished game"
    (assertEqual False
      (gameOver (getOActiveStateFor test3x3Board)))

  ]

boardA : List (List Cell)
boardA =
  [ [X, O, X]
  , [O, O, Empty]
  , [O, X, X]
  ]

gameAState : GameState
gameAState =
  { board = boardA,
    activePlayer = X,
    inactivePlayer = O,
    winner = Empty }

boardB : List (List Cell)
boardB =
  [ [X, O, X]
  , [O, O, X]
  , [O, X, X]
  ]

gameBState : GameState
gameBState =
  { board = boardB,
    activePlayer = O,
    inactivePlayer = X,
    winner = Empty }

minimaxTest : Test
minimaxTest =
  suite
  "Test Minimax & Whole-Board-Scoring"
  [
    test
    "nextState correct board"
    (assertEqual gameBState
      (nextState gameAState {x=1, y=2}))

  , test
    "chooses winning move from one choice"
    (assertEqual 8
      (minimaxMove (getOActiveStateFor oneEmptySpace)))

  , test
    "chooses winning move from board with many empty spaces"
      (assertEqual 8
        (minimaxMove (getOActiveStateFor nearWin)))

  , test
    "selects losing move if that's all there is"
    (assertEqual 0
      (minimaxMove (getXActiveStateFor xWinsOnZero)))

  , test
    "takes winning move over block if available"
    (assertEqual 8
      (minimaxMove (getOActiveStateFor blockOrWin)))

  , test
    "returns neutral score for cats game"
    (assertEqual 0
      (minimaxMove (getOActiveStateFor catBoard)))

  , test
    "blocks opponent at position 0"
    (assertEqual 0
      (minimaxMove (getOActiveStateFor blockAt0)))

  , test
    "blocks opponent at position 0 vertically "
    (assertEqual 0
      (minimaxMove (getOActiveStateFor blockAt0Vertically)))

  , test
    "blocks opponent at position 0 diagonally "
    (assertEqual 0
      (minimaxMove (getOActiveStateFor blockAt0Diagonally)))

  , test
    "blocks opponent at position 1"
    (assertEqual 1
      (minimaxMove (getOActiveStateFor blockAt1)))

  , test
    "blocks opponent at position 1 vertically"
    (assertEqual 1
      (minimaxMove (getOActiveStateFor blockAt1Vertically)))

  , test
    "blocks opponent at position 2"
    (assertEqual 2
      (minimaxMove (getOActiveStateFor blockAt2)))

  , test
    "blocks opponent at position 2 vertically "
    (assertEqual 2
      (minimaxMove (getOActiveStateFor blockAt2Vertically)))

  , test
    "blocks opponent at position 2 diagonally "
    (assertEqual 2
      (minimaxMove (getOActiveStateFor blockAt2Diagonally)))

  , test
    "blocks opponent at position 3"
    (assertEqual 3
      (minimaxMove (getOActiveStateFor blockAt3)))

  , test
    "blocks opponent at position 3 vertically"
    (assertEqual 3
      (minimaxMove (getOActiveStateFor blockAt3Vertical)))

  , test
    "blocks opponent at position 4"
    (assertEqual 4
      (minimaxMove (getOActiveStateFor blockAt4)))

  , test
    "blocks opponent at position 4 vertically"
    (assertEqual 4
      (minimaxMove (getOActiveStateFor blockAt4Vertical)))

  , test
    "blocks opponent at position 5"
    (assertEqual 5
      (minimaxMove (getOActiveStateFor blockAt5)))

  , test
    "blocks opponent at position 5 vertically"
    (assertEqual 5
      (minimaxMove (getOActiveStateFor blockAt5Vertical)))

  , test
    "blocks opponent at position 6"
    (assertEqual 6
      (minimaxMove (getOActiveStateFor blockAt6)))

  , test
    "blocks opponent at position 6 vertically"
    (assertEqual 6
      (minimaxMove (getOActiveStateFor blockAt6Vertical)))

  , test
    "blocks opponent at position 6 diagonally"
    (assertEqual 6
      (minimaxMove (getOActiveStateFor blockAt6Diagonal)))

  , test
    "blocks opponent at position 7"
    (assertEqual 7
      (minimaxMove (getOActiveStateFor blockAt7)))

  , test
    "blocks opponent at position 7 vertically"
    (assertEqual 7
      (minimaxMove (getOActiveStateFor blockAt7Vertical)))

  , test
    "blocks opponent at position 8"
    (assertEqual 8
      (minimaxMove (getOActiveStateFor blockAt8)))

  , test
    "blocks opponent at position 8 vertically"
    (assertEqual 8
      (minimaxMove (getOActiveStateFor blockAt8Vertical)))

  , test
    "blocks opponent in middle of board"
    (assertEqual 5
      (minimaxMove (getOActiveStateFor middleBlockBoard)))

  , test
    "blocks at bottom of board"
    (assertEqual 8
      (minimaxMove (getOActiveStateFor bottomBlockBoard)))

  , test
    "blocks even if no win possible"
    (assertEqual 7
      (minimaxMove (getOActiveStateFor centerBlockOrFailBoard)))
  ]
xInCenterBoard: List (List Cell)
xInCenterBoard =
  [ [Empty, Empty, Empty],
    [Empty,     X, Empty],
    [Empty, Empty, Empty]]

------- Every possible block:

blockAt0 : List (List Cell)
blockAt0 =
  [ [Empty,    X,    X],
    [Empty,Empty,Empty],
    [Empty,Empty,Empty] ]

blockAt0Vertically : List (List Cell)
blockAt0Vertically =
  [ [Empty,Empty,Empty],
    [    X,Empty,Empty],
    [    X,Empty,Empty] ]

blockAt0Diagonally : List (List Cell)
blockAt0Diagonally =
  [ [Empty,Empty,Empty],
    [Empty,    X,Empty],
    [Empty,Empty,    X] ]

blockAt1 : List (List Cell)
blockAt1 =
  [ [    X,Empty,    X],
    [Empty,Empty,Empty],
    [Empty,Empty,Empty] ]

blockAt1Vertically : List (List Cell)
blockAt1Vertically =
  [ [Empty,Empty,Empty],
    [Empty,    X,Empty],
    [Empty,    X,Empty] ]

blockAt2 : List (List Cell)
blockAt2 =
  [ [    X,    X,Empty],
    [Empty,Empty,Empty],
    [Empty,Empty,Empty] ]

blockAt2Vertically : List (List Cell)
blockAt2Vertically =
  [ [Empty,Empty,Empty],
    [Empty,Empty,    X],
    [Empty,Empty,    X] ]

blockAt2Diagonally : List (List Cell)
blockAt2Diagonally =
  [ [Empty,Empty,Empty],
    [Empty,    X,Empty],
    [    X,Empty,Empty] ]

blockAt3 : List (List Cell)
blockAt3 =
  [ [Empty,Empty,Empty],
    [Empty,    X,    X],
    [Empty,Empty,Empty] ]

blockAt3Vertical : List (List Cell)
blockAt3Vertical =
  [ [    X,Empty,Empty],
    [Empty,Empty,Empty],
    [    X,Empty,Empty] ]

blockAt4 : List (List Cell)
blockAt4 =
  [ [Empty,Empty,Empty],
    [    X,Empty,    X],
    [Empty,Empty,Empty] ]

blockAt4Vertical : List (List Cell)
blockAt4Vertical =
  [ [Empty,    X,Empty],
    [Empty,Empty,Empty],
    [Empty,    X,Empty] ]

blockAt5 : List (List Cell)
blockAt5 =
  [ [Empty,Empty,Empty],
    [    X,    X,Empty],
    [Empty,Empty,Empty] ]

blockAt5Vertical : List (List Cell)
blockAt5Vertical =
  [ [Empty,Empty,    X],
    [Empty,Empty,Empty],
    [Empty,Empty,    X] ]

blockAt6 : List (List Cell)
blockAt6 =
  [ [Empty,Empty,Empty],
    [Empty,Empty,Empty],
    [Empty,    X,    X] ]

blockAt6Vertical : List (List Cell)
blockAt6Vertical =
  [ [    X,Empty,Empty],
    [    X,Empty,Empty],
    [Empty,Empty,Empty] ]

blockAt6Diagonal : List (List Cell)
blockAt6Diagonal =
  [ [Empty,Empty,    X],
    [Empty,    X,Empty],
    [Empty,Empty,Empty] ]

blockAt7 : List (List Cell)
blockAt7 =
  [ [Empty,Empty,Empty],
    [Empty,Empty,Empty],
    [    X,Empty,    X] ]

blockAt7Vertical : List (List Cell)
blockAt7Vertical =
  [ [Empty,    X,Empty],
    [Empty,    X,Empty],
    [Empty,Empty,Empty] ]

blockAt8 : List (List Cell)
blockAt8 =
  [ [Empty,Empty,Empty],
    [Empty,Empty,Empty],
    [    X,    X,Empty] ]

blockAt8Vertical : List (List Cell)
blockAt8Vertical =
  [ [Empty,Empty,    X],
    [Empty,Empty,    X],
    [Empty,Empty,Empty] ]

------------------------

blockOrWin : List (List Cell)
blockOrWin =
  [ [    X,     X, Empty]
  , [Empty, Empty, Empty]
  , [    O,     O, Empty]
  ]

middleBlockBoard : List (List Cell)
middleBlockBoard =
  [ [Empty, Empty, Empty]
  , [X,         X, Empty]
  , [Empty, Empty, O]
  ]

bottomBlockBoard : List (List Cell)
bottomBlockBoard =
  [ [Empty, Empty, Empty]
  , [Empty,     O, Empty]
  , [    X,     X, Empty]
  ]

centerBlockOrFailBoard : List (List Cell)
centerBlockOrFailBoard =
  [ [    O,     X, O]
  , [    X,     X, O]
  , [Empty, Empty, X]
  ]

catBoard : List (List Cell)
catBoard =
  [ [X,X,O],
    [O,X,X],
    [X,O,O] ]

multipleChoice : List (List Cell)
multipleChoice =
  [ [    X, X, O]
  , [Empty, O, O]
  , [Empty, X, X]
  ]

xWinsOnZero : List (List Cell)
xWinsOnZero =
  [ [Empty, X, O]
  , [    X, X, O]
  , [    O, O, X]
  ]

