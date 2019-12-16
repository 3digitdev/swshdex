module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error)
import Json.Decode as JD exposing (decodeString, dict, list, string)
import List.Extra as LX exposing (find)
import String exposing (join, split)


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


type alias Pokemon =
    { name : String
    , number : String
    , pokeType : PokemonType
    }


type Mode
    = TypeInfo
    | DualTypeMatchup
    | Pokedex


type alias Model =
    { allTypes : List Type
    , allPokemon : List Pokemon
    , searchResults : List Pokemon
    , selectedTypes : ( Maybe String, Maybe String )
    , mode : Mode
    , currentType : Maybe Type
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
                                JD.fail "Invalid PokemonType"
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
    , selectedTypes = ( Just "Bug", Just "Steel" )
    , mode = DualTypeMatchup
    , currentType = Maybe.Nothing
    , currentPokemon = Maybe.Nothing
    }



-- UPDATE


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
            ( { model | mode = mode }, Cmd.none )

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
                    model.allPokemon
                        |> List.filter (\pokemon -> pokemon.name |> String.startsWith searchStr)
            in
            ( { model | searchResults = results }, Cmd.none )

        ChangeCurrentType typeName ->
            ( { model | currentType = LX.find (\t -> t.name == typeName) model.allTypes }, Cmd.none )

        SelectType typeName ->
            ( model |> updatedSelectedTypes typeName, Cmd.none )

        BackToTypeList ->
            ( { model | currentType = Maybe.Nothing }, Cmd.none )


type Msg
    = LoadData
    | ChangeMode Mode
    | TypesLoaded (Result Http.Error (List Type))
    | PokemonLoaded (Result Http.Error (List Pokemon))
    | SearchPokedex String
    | ChangeCurrentType String
    | SelectType String
    | BackToTypeList
    | NoOp


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
    { model | selectedTypes = newTypes }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


renderTypeInfo : Type -> Html Msg
renderTypeInfo curType =
    let
        noEffect =
            case curType.noEffects of
                [] ->
                    div [] []

                typeList ->
                    div []
                        [ h2 [] [ text "Has no effect on:" ]
                        , h3 [] [ text (join ", " typeList) ]
                        ]
    in
    div [ class "types" ]
        [ button [ onClick BackToTypeList ] [ text "<- Back to Types" ]
        , h1 [] [ text (curType.name ++ " Type:") ]
        , h2 [] [ text "Super effective against:" ]
        , h3 [] [ text (join ", " curType.strengths) ]
        , h2 [] [ text "Weak to:" ]
        , h3 [] [ text (join ", " curType.weaknesses) ]
        , h2 [] [ text "Not very effective against:" ]
        , h3 [] [ text (join ", " curType.ineffectives) ]
        , noEffect
        ]


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


renderTypeButton : String -> Model -> Html Msg
renderTypeButton typeStr model =
    let
        ( tdClass, clickFn ) =
            case model.mode of
                DualTypeMatchup ->
                    if typeStr |> typeIsSelected model.selectedTypes then
                        ( "type-link type-selected", SelectType )

                    else
                        ( "type-link", SelectType )

                _ ->
                    case model.currentType of
                        Nothing ->
                            ( "type-link", ChangeCurrentType )

                        Just a ->
                            if a.name == typeStr then
                                ( "type-link type-selected", ChangeCurrentType )

                            else
                                ( "type-link", ChangeCurrentType )
    in
    td [ class tdClass, onClick (clickFn typeStr) ]
        [ text typeStr ]


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
            div [ class "types" ]
                [ table [ class "type-table" ]
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


renderPokedex : Model -> Html Msg
renderPokedex model =
    div [ class "pokedex" ]
        [ h1 [] [ text "Pokemon" ]
        , input
            [ class "dex-search"
            , onInput SearchPokedex
            , height 50
            , type_ "text"
            ]
            []
        , ul [] (List.map (\result -> li [] [ text result.name ]) model.searchResults)
        ]


renderDualTypeInfo : Model -> Html Msg
renderDualTypeInfo model =
    case model.selectedTypes of
        ( Just one, Just two ) ->
            div []
                [ h3 [] [ text ("Dual Type: " ++ one ++ "/" ++ two) ]
                ]

        _ ->
            div [] []


view : Model -> Html Msg
view model =
    let
        renderTypeMatchup =
            case model.currentType of
                Just typeData ->
                    renderTypeInfo typeData

                Maybe.Nothing ->
                    renderTypeList model

        renderFn =
            case model.mode of
                TypeInfo ->
                    renderTypeMatchup

                DualTypeMatchup ->
                    div []
                        [ renderTypeList model
                        , renderDualTypeInfo model
                        ]

                Pokedex ->
                    renderPokedex model
    in
    div [ class "container" ]
        [ renderFn
        ]
