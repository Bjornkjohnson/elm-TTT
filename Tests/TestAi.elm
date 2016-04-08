module TestAi (..) where

import ElmTest exposing (..)
import Ai exposing (..)
import DataTypes exposing (..)
import BoardHelpers exposing (..)
import Dict exposing (..)


nearWinGameState =
  { board = nearWin,
    activePlayer = O,
    inactivePlayer = X }

oneEmptySpaceState =
  { board = oneEmptySpace,
    activePlayer = O,
    inactivePlayer = X }


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
    "selects max scoring cell if currentMarker is computer marker"
    (assertEqual (Just 0) (getMinOrMax (Dict.fromList testScores) DataTypes.O))

  , test
    "selects min scoring cell if currentMarker is opponent marker"
    (assertEqual (Just 1) (getMinOrMax (Dict.fromList testScores) DataTypes.X))

  , test
    "getMaxValue gets max"
    (assertEqual (Just 10) (getMaxValue (Dict.fromList testScores)))

  , test
    "getMinValue gets max"
    (assertEqual (Just -10) (getMinValue (Dict.fromList testScores)))

  , test
    "getValuesKey gets the value of its key"
    (assertEqual (Just 0) (getValuesKey (Dict.fromList testScores) (Just 10)))

  , test
    "emptySpaces returns empty spaces - nearly full board"
    (assertEqual [{x=2, y=2}] (getEmptySpacesInBoard(oneEmptySpace)))


  , test
    "emptySpaces returns empty spaces - nearly empty board"
    (assertEqual [{x=0, y=0}, {x=0, y=1},
                  {x=1, y=0}, {x=1, y=1},
                  {x=2, y=0}, {x=2, y=1}, {x=2, y=2}]
      (getEmptySpacesInBoard(nearWin)))


  , test
    "emptySpacesRow returns empty spaces"
    (assertEqual [{x=0, y=1}, {x=0, y=2}] (getEmptySpacesInRow((0, [X, Empty, Empty]))))

  , test
    "flatten-board flattens lists"
    (assertEqual [X, X, O, Empty] (flattenBoard [[X, X], [O, Empty]]))

  , test
    "breaks it up be index"
    (assertEqual [(0, "please"), (1, "no")]
      (indexedElements(["please", "no"])))

  ]

minimaxTest : Test
minimaxTest =
  suite
  "Test Minimax"
  [ test
    "wins on next move if possible"
    (assertEqual 8 (minimaxMove nearWinGameState))
  ]