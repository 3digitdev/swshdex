#!/bin/bash

# Standard building

# Compile Elm
elm make src/Main.elm --optimize --output=public/elm.js

# Debug Commands
# elm make src/Main.elm --output=public/elm.js
# elm reactor
