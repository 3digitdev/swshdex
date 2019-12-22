#!/bin/bash
# elm make src/Main.elm --output=public/index.js
elm make src/Main.elm --optimize --output=public/index.js
elm reactor
