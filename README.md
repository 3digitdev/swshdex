# Sw/Sh Dex

Pokedex and helper app for Pokemon Sword/Shield

**Hosted at https://dex.3digit.dev**

## TODO
- Clean up CSS a bit...
- Add a Port so JS can allow closing the modal by clicking outside the box
- Add "info" icons to each feature with a modal popup of what it is
- Add "suggested pokemon" (maybe 5?) to the bottom of the type matchup page
- Enable the Party Planner to support type-coverage checking of DUAL types.
- Split up `Main.elm` into separate parts

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
