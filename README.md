# Sw/Sh Dex

Pokedex and helper app for Pokemon Sword/Shield

**Hosted at https://dex.3digit.dev**

## TODO
- Add "info" icons to each feature with a modal popup of what it is
  - Before I can do this, I need an "info" icon to use!
    - Either an `i` or a `?` in a circle.
    - Needs to match NES.css style!

## Credits

CSS Styling was done using [NES.css](https://nostalgic-css.github.io/NES.css/)
with only minor modifications made for my own purpose.  Credit for that styling goes
to the author of NES.css

## Tech

**Front-end:** App is created entirely using [Elm](https://elm-lang.org), with a few custom-built JSON
files for my data.  No back-end component.

**Hosting:** Hosted on [Firebase](https://firebase.google.com/).

**Data/API:** Future intention is to use the [PokeAPI](https://pokeapi.co) for the data, but as of
12/18/2019 they do not support Sword/Shield data.  When they update, I plan to use that
data if possible.

## Run it locally

1. [Install Elm](https://guide.elm-lang.org/install/elm.html)
2. Clone the repo locally
3. Navigate to the project root and run `bash buildElm.sh` (Mac/Linux only)
  - If you are on windows, you can instead run:
    - `elm make src/Main.elm --optimize --output=public/index.js && elm reactor`
4. Navigate in a browser to http://localhost:8080/public/index.html
5. Voila!
