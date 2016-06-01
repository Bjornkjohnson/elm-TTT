module Tests (..) where

import ElmTest exposing (..)

baseTest : Test
baseTest =
  suite
  "Welcome to Your Tests!"
  [ test
    "hello!"
    (assertEqual True True)]
