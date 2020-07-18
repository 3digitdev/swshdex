#!/bin/bash

# Standard building

# Compile Elm
elm make src/Main.elm --optimize --output=public/index.js

# Debug Commands
# elm make src/Main.elm --output=public/index.js
# elm reactor
