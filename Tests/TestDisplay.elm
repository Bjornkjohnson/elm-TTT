module TestDisplay (..) where

import ElmTest exposing (..)
import Display exposing (..)
import DataTypes exposing (..)

displayTest : Test
displayTest =
  suite
    "Test for behavioral changes to display functions"
    [ test
        "Test converting Empty Cell to String"
        (assertEqual "5" (convertCellToString 4 Empty))
    , test
        "Test converting X Cell to String"
        (assertEqual "X" (convertCellToString 0 X))
    , test
        "Test converting O Cell to String"
        (assertEqual "O" (convertCellToString 0 O))
    , test
        "Build Winner String X"
        (assertEqual "X Wins" (buildWinnerString X))
    , test
        "Build Winner String O"
        (assertEqual "O Wins" (buildWinnerString O))
    , test
        "Build no Winner String"
        (assertEqual "" (buildWinnerString Empty))
    ]
