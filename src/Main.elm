module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error, expectJson, get)
import Json.Decode as JD exposing (..)
import List.Extra as LX exposing (gatherEqualsBy, groupsOf, uniqueBy)
import String exposing (join, split)
import Task exposing (perform, succeed)
import Tuple2 as TX exposing (uncurry)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODELS/TYPES


type PokemonType
    = Single String
    | Dual String String


type alias Type =
    { name : String
    , strengths : List String
    , weaknesses : List String
    , ineffectives : List String
    , noEffects : List String
    }


type alias Defenses =
    { x4 : List String
    , x2 : List String
    , x0 : List String
    , half : List String
    , quarter : List String
    }


type alias Pokemon =
    { name : String
    , number : String
    , pokeType : PokemonType
    }


type Mode
    = TypeInfo
    | Pokedex


type alias Model =
    { allTypes : List Type
    , allPokemon : List Pokemon
    , searchResults : List Pokemon
    , selectedTypes : ( Maybe String, Maybe String )
    , mode : Mode
    , currentTypeDefenses : Defenses
    , currentPokemon : Maybe Pokemon
    }



-- Loading Data


decodeTypeData : JD.Decoder Type
decodeTypeData =
    JD.map5 Type
        (JD.field "name" JD.string)
        (JD.field "strengths" (JD.list JD.string))
        (JD.field "weaknesses" (JD.list JD.string))
        (JD.field "ineffectives" (JD.list JD.string))
        (JD.field "no_effects" (JD.list JD.string))


decodePokemonData : JD.Decoder Pokemon
decodePokemonData =
    JD.map3 Pokemon
        (JD.field "name" JD.string)
        (JD.field "number" JD.string)
        (JD.field "pokeType"
            (JD.string
                |> JD.andThen
                    (\typeStr ->
                        case split "/" typeStr of
                            [ typeOne, typeTwo ] ->
                                JD.succeed (Dual typeOne typeTwo)

                            [ typeName ] ->
                                JD.succeed (Single typeName)

                            _ ->
                                JD.fail ("Invalid PokemonType " ++ typeStr)
                    )
            )
        )


getDataList : String -> JD.Decoder a -> (Result Http.Error (List a) -> Msg) -> Cmd Msg
getDataList fileName decoder cmd =
    Http.get
        { url = fileName ++ ".json"
        , expect = Http.expectJson cmd (JD.list decoder)
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    update LoadData initModel


initModel : Model
initModel =
    { allTypes = []
    , allPokemon = []
    , searchResults = []
    , selectedTypes = ( Nothing, Nothing )
    , mode = Pokedex
    , currentTypeDefenses = { x4 = [], x2 = [], x0 = [], half = [], quarter = [] }
    , currentPokemon = Maybe.Nothing
    }



-- UPDATE


type Msg
    = LoadData
    | ChangeMode Mode
    | TypesLoaded (Result Http.Error (List Type))
    | PokemonLoaded (Result Http.Error (List Pokemon))
    | SearchPokedex String
    | SelectType String
    | SetType PokemonType
    | ResetTypeSelections
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LoadData ->
            ( model
            , Cmd.batch
                [ getDataList "poketypes" decodeTypeData TypesLoaded
                , getDataList "pokedex" decodePokemonData PokemonLoaded
                ]
            )

        ChangeMode mode ->
            ( { model | mode = mode, selectedTypes = ( Nothing, Nothing ), searchResults = [] }, Cmd.none )

        TypesLoaded result ->
            case result of
                Err httpError ->
                    ( model, Cmd.none )

                Ok typeData ->
                    ( { model | allTypes = typeData }, Cmd.none )

        PokemonLoaded result ->
            case result of
                Err httpError ->
                    ( model, Cmd.none )

                Ok pokeData ->
                    ( { model | allPokemon = pokeData }, Cmd.none )

        SearchPokedex searchStr ->
            let
                results =
                    case searchStr of
                        "" ->
                            []

                        _ ->
                            model.allPokemon |> findMatchByName searchStr
            in
            ( { model | searchResults = results }, Cmd.none )

        SelectType typeName ->
            ( model |> updatedSelectedTypes typeName, Cmd.none )

        SetType pokeType ->
            let
                ( newModel, nextType ) =
                    case pokeType of
                        Single t ->
                            ( { model | selectedTypes = ( Nothing, Nothing ) }, t )

                        Dual one two ->
                            ( { model | selectedTypes = ( Just one, Nothing ) }, two )
            in
            ( { newModel | mode = TypeInfo }
            , nextType
                |> Task.succeed
                |> Task.perform SelectType
            )

        ResetTypeSelections ->
            ( { model | selectedTypes = ( Nothing, Nothing ) }, Cmd.none )



-- Support functions for UPDATE


findMatchByName : String -> List Pokemon -> List Pokemon
findMatchByName searchTerm pokemonList =
    let
        matchFront =
            pokemonList
                |> List.filter
                    (\p ->
                        String.toLower p.name
                            |> String.startsWith (String.toLower searchTerm)
                    )

        matchAnywhere =
            pokemonList
                |> List.filter
                    (\p ->
                        String.toLower p.name
                            |> String.contains (String.toLower searchTerm)
                    )
    in
    -- Match on "start of string" first, but show any results
    case String.length searchTerm of
        1 ->
            matchFront

        _ ->
            (matchFront ++ matchAnywhere) |> LX.uniqueBy (\r -> r.number)


updatedSelectedTypes : String -> Model -> Model
updatedSelectedTypes newType model =
    let
        ( one, two ) =
            model.selectedTypes

        newTypes =
            case ( one, two ) of
                ( Nothing, Nothing ) ->
                    -- No types selected
                    ( Just newType, Nothing )

                ( Just a, Nothing ) ->
                    if a == newType then
                        -- The one selected is the same; untoggle
                        ( Nothing, Nothing )

                    else
                        -- Set second type
                        ( Just a, Just newType )

                ( Nothing, Just a ) ->
                    if a == newType then
                        -- The one selected is the same; untoggle
                        ( Nothing, Nothing )

                    else
                        -- Set first type
                        ( Just newType, Just a )

                ( Just a, Just b ) ->
                    if a == newType then
                        -- The one clicked is set; untoggle
                        ( Nothing, Just b )

                    else if b == newType then
                        -- The one clicked is set; untoggle
                        ( Just a, Nothing )

                    else
                        -- Something went wrong; don't modify anything
                        ( Just a, Just b )
    in
    newTypes |> calculateDefenses { model | selectedTypes = newTypes }


getBoolValue : List String -> String -> Int
getBoolValue typeList typeName =
    if typeList |> List.member typeName then
        1

    else
        0


assignByValue : ( Int, List String ) -> Defenses -> Defenses
assignByValue ( val, typeList ) defenses =
    case val of
        0 ->
            { defenses | x4 = typeList }

        1 ->
            { defenses | x2 = typeList }

        3 ->
            { defenses | half = typeList }

        4 ->
            { defenses | quarter = typeList }

        5 ->
            { defenses | x0 = typeList }

        _ ->
            defenses


calculateDefenses : Model -> ( Maybe String, Maybe String ) -> Model
calculateDefenses model bothTypes =
    let
        dualType =
            bothTypes |> Tuple.mapBoth (Maybe.withDefault "") (Maybe.withDefault "")

        sortedMap =
            model.allTypes
                |> List.map
                    (\curType ->
                        let
                            bad =
                                dualType
                                    |> Tuple.mapBoth (getBoolValue curType.strengths) (getBoolValue curType.strengths)
                                    |> TX.uncurry (+)

                            good =
                                dualType
                                    |> Tuple.mapBoth (getBoolValue curType.ineffectives) (getBoolValue curType.ineffectives)
                                    |> TX.uncurry (+)

                            noEffect =
                                dualType
                                    |> Tuple.mapBoth (\a -> List.member a curType.noEffects) (\a -> List.member a curType.noEffects)
                                    |> TX.uncurry (||)
                        in
                        if noEffect then
                            ( curType.name, 5 )

                        else
                            ( curType.name, good - bad + 2 )
                    )
                |> LX.gatherEqualsBy Tuple.second
                |> List.map
                    (\valueSet ->
                        Tuple.pair
                            (Tuple.second
                                (Tuple.first valueSet)
                            )
                            ([ Tuple.first (Tuple.first valueSet) ]
                                ++ List.map (\a -> Tuple.first a) (Tuple.second valueSet)
                            )
                    )
                |> List.sortBy Tuple.first
    in
    { model
        | currentTypeDefenses =
            sortedMap
                |> List.foldl assignByValue (Defenses [] [] [] [] [])
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        renderFn =
            case model.mode of
                TypeInfo ->
                    [ div [ class "btn-container" ]
                        [ button [ class "nes-btn reset-btn", onClick ResetTypeSelections ] [ text "RESET" ]
                        , button [ class "nes-btn", onClick (ChangeMode Pokedex) ] [ text "POKEDEX" ]
                        ]
                    , renderTypeList model
                    , renderTypeInfo model
                    ]

                Pokedex ->
                    [ div [ class "btn-container" ]
                        [ button
                            [ class "nes-btn", onClick (ChangeMode TypeInfo) ]
                            [ text "TYPE MATCHUPS" ]
                        ]
                    , renderPokedex model
                    ]
    in
    div [ class "container" ]
        renderFn



-- TYPE MATCHUP VIEW


renderTypeList : Model -> Html Msg
renderTypeList model =
    case model.allTypes of
        [] ->
            div [] []

        typeData ->
            let
                grouped =
                    typeData |> LX.groupsOf 2
            in
            div [ class "type-table nes-table-responsive" ]
                [ table [ class "nes-table is-bordered is-centered" ]
                    (List.map
                        (\two ->
                            tr []
                                (List.map
                                    (\one ->
                                        model |> renderTypeButton one.name
                                    )
                                    two
                                )
                        )
                        grouped
                    )
                ]


renderTypeButton : String -> Model -> Html Msg
renderTypeButton typeStr model =
    let
        ( innerText, clickFn ) =
            case model.mode of
                TypeInfo ->
                    if typeStr |> typeIsSelected model.selectedTypes then
                        ( "> " ++ typeStr ++ " <", SelectType )

                    else
                        ( typeStr, SelectType )

                _ ->
                    ( typeStr, SelectType )
    in
    td [ class "type-link", onClick (clickFn typeStr) ]
        [ text innerText ]


typeIsSelected : ( Maybe String, Maybe String ) -> String -> Bool
typeIsSelected ( one, two ) typeStr =
    case ( one, two ) of
        ( Nothing, Nothing ) ->
            False

        ( Just a, Nothing ) ->
            a == typeStr

        ( Nothing, Just a ) ->
            a == typeStr

        ( Just a, Just b ) ->
            a == typeStr || b == typeStr


renderTypeInfo : Model -> Html Msg
renderTypeInfo model =
    case model.selectedTypes of
        ( Just one, Just two ) ->
            div [ class "nes-container with-title is-rounded" ]
                [ p [ class "title" ] [ text ("Dual Type: " ++ one ++ "/" ++ two) ]
                , renderDefenses model
                ]

        ( Just typeName, Nothing ) ->
            typeName |> renderSingleTypeInfo model

        ( Nothing, Just typeName ) ->
            typeName |> renderSingleTypeInfo model

        _ ->
            div [] []


renderDefenses : Model -> Html Msg
renderDefenses model =
    div []
        [ h3 [] [ text "Weaknesses:" ]
        , renderDefenseInfoSet model.currentTypeDefenses.x4 "4x"
        , renderDefenseInfoSet model.currentTypeDefenses.x2 "2x"
        , h3 [] [ text "Strengths:" ]
        , renderDefenseInfoSet model.currentTypeDefenses.half "1/2"
        , renderDefenseInfoSet model.currentTypeDefenses.quarter "1/4"
        , h3 [] [ text "Immunities:" ]
        , renderDefenseInfoSet model.currentTypeDefenses.x0 "NO"
        ]


renderDefenseInfoSet : List String -> String -> Html Msg
renderDefenseInfoSet typeList modifier =
    case typeList of
        [] ->
            div [] []

        items ->
            p []
                [ strong [] [ text (modifier ++ " damage from: ") ]
                , renderBadgeList items
                ]


renderSingleTypeInfo : Model -> String -> Html Msg
renderSingleTypeInfo model typeName =
    let
        foundType =
            model.allTypes
                |> List.filter (\t -> t.name == typeName)
                |> List.head
    in
    case foundType of
        Nothing ->
            div [] []

        Just typeInfo ->
            div [ class "nes-container with-title is-rounded" ]
                [ p [ class "title" ] [ text (typeName ++ " Type") ]
                , renderSingleInfoSet typeInfo.strengths "Super effective against:"
                , renderSingleInfoSet typeInfo.ineffectives "Not very effective against:"
                , renderSingleInfoSet typeInfo.weaknesses "Weak to:"
                , renderSingleInfoSet typeInfo.noEffects "Has no effect on:"
                ]


renderSingleInfoSet : List String -> String -> Html Msg
renderSingleInfoSet typeList preText =
    case typeList of
        [] ->
            div [] []

        items ->
            div []
                [ h4 [] [ text preText ]
                , renderBadgeList items
                ]


renderPokedex : Model -> Html Msg
renderPokedex model =
    div []
        [ h1 [] [ text "Pokemon" ]
        , div [ class "nes-field" ]
            [ label [ for "search-box" ]
                [ text "Search:" ]
            , input
                [ class "nes-input dex-search"
                , onInput SearchPokedex
                , id "search-box"
                , type_ "text"
                ]
                []
            ]
        , p [ class "search-hint" ] [ text "Hint:  Click a Pokemon's type(s) to jump to type matchups!" ]
        , ul [] (List.map renderPokemon model.searchResults)
        ]


renderPokemon : Pokemon -> Html Msg
renderPokemon pokemon =
    li [ class "search-result-item" ]
        [ text (pokemon.number ++ ": ")
        , strong [] [ text pokemon.name ]
        , div
            [ class "type-link"
            , onClick (SetType pokemon.pokeType)
            ]
            [ pokemon.pokeType |> renderTypeBadge ]
        ]



-- Rendering Type Badges


renderBadgeList : List String -> Html Msg
renderBadgeList typeList =
    div [ class "badge-container" ]
        (typeList
            |> List.map (\i -> renderTypeBadge (Single i))
        )


renderTypeBadge : PokemonType -> Html Msg
renderTypeBadge pokeType =
    case pokeType of
        Single t ->
            a [ class "nes-badge" ]
                [ span
                    [ class (String.toLower t ++ "-badge") ]
                    [ text t ]
                ]

        Dual one two ->
            a [ class "nes-badge" ]
                [ span [ class (String.toLower one ++ "-badge") ]
                    [ text one ]
                , span [ class (String.toLower two ++ "-badge") ]
                    [ text two ]
                ]
