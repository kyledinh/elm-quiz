#!/bin/bash

elm-format src/Quiz.elm --yes
elm-format src/Model.elm --yes
elm-format src/Data.elm --yes
elm-format src/Util.elm --yes
elm make src/Quiz.elm --output=elm.js
