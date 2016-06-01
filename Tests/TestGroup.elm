module TestGroup (..) where

import ElmTest exposing (..)
import Tests
import TestBoard exposing (..)
import TestGame exposing (..)
import TestDisplay exposing (..)
import TestAi exposing (..)

tests : Test
tests =
  suite
    "Tic Tac Toe Tests"
    [ TestBoard.boardTests,
      TestGame.gameTests,
      TestDisplay.displayTest,
      TestAi.aiInfrastructureTest,
      TestAi.minimaxTest
    ]
