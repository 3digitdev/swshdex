-- TODO:  MAKE A PORT FOR CLICKING OUTSIDE MODAL!
-- https://www.w3schools.com/howto/tryit.asp?filename=tryhow_css_modal


module Main exposing (main)

import Array exposing (get, repeat, set)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error, expectJson, get)
import Json.Decode as JD exposing (..)
import List.Extra as LX exposing (gatherEqualsBy, groupsOf, uniqueBy)
import Random exposing (generate)
import Random.List as RandList exposing (choose)
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


type alias PokemonParty =
    { pokemonList : Array.Array (Maybe Pokemon)
    , total : Int
    , suggestedPokemon : Maybe Pokemon
    }


type alias ModalData =
    { searchText : String
    , index : Int
    }


type Mode
    = TypeInfo
    | Pokedex
    | Party


type alias Model =
    { allTypes : List Type
    , allPokemon : List Pokemon
    , searchResults : List Pokemon
    , selectedTypes : ( Maybe String, Maybe String )
    , suggestedPokemon : Maybe Pokemon
    , mode : Mode
    , currentTypeDefenses : Defenses
    , currentParty : PokemonParty
    , modalData : Maybe ModalData
    , partyModalInputTxt : String
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
    , suggestedPokemon = Nothing
    , mode = Pokedex
    , currentTypeDefenses = { x4 = [], x2 = [], x0 = [], half = [], quarter = [] }
    , currentParty = initParty
    , modalData = Nothing
    , partyModalInputTxt = ""
    }


initParty : PokemonParty
initParty =
    { pokemonList = Array.repeat 6 Nothing
    , total = 0
    , suggestedPokemon = Nothing
    }



-- UPDATE


type Msg
    = NoOp
    | LoadData
    | ChangeMode Mode
    | TypesLoaded (Result Http.Error (List Type))
    | PokemonLoaded (Result Http.Error (List Pokemon))
    | SearchPokedex String
    | SelectType String
    | SetType PokemonType
    | ResetTypeSelections
    | AddPartyMemberAt Int
    | ConfirmPartyMember Int (Maybe Pokemon)
    | ClearPartyMemberAt Int
    | FindPartyMember String
    | CloseModal
    | RandomizeSuggestion
    | NewSuggestion ( Maybe Pokemon, List Pokemon )


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
            ( { model | mode = mode, searchResults = [] }, Cmd.none )

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
            ( model |> updatedSelectedTypes typeName |> suggestFirstPokemon, Cmd.none )

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

        AddPartyMemberAt idx ->
            ( { model | modalData = Just { index = idx, searchText = "" }, searchResults = [] }, Cmd.none )

        ConfirmPartyMember idx pokemon ->
            ( model |> closeModalAndUpdateWith pokemon idx, Cmd.none )

        ClearPartyMemberAt idx ->
            ( model |> closeModalAndUpdateWith Nothing idx, Cmd.none )

        FindPartyMember searchStr ->
            let
                results =
                    case searchStr of
                        "" ->
                            []

                        _ ->
                            model.allPokemon
                                |> findMatchByName searchStr
                                |> List.filter
                                    (\p ->
                                        model.currentParty.pokemonList
                                            |> Array.toList
                                            |> List.member (Just p)
                                            |> not
                                    )

                oldData =
                    model.modalData

                newData =
                    case oldData of
                        Nothing ->
                            Nothing

                        Just data ->
                            Just { data | searchText = searchStr }
            in
            ( { model | modalData = newData, searchResults = results }, Cmd.none )

        CloseModal ->
            ( { model | modalData = Nothing, searchResults = [] }, Cmd.none )

        RandomizeSuggestion ->
            ( model, Random.generate NewSuggestion (RandList.choose (suggestPokemon model)) )

        NewSuggestion ( pokemon, _ ) ->
            ( { model | suggestedPokemon = pokemon }, Cmd.none )



-- Support functions for UPDATE


closeModalAndUpdateWith : Maybe Pokemon -> Int -> Model -> Model
closeModalAndUpdateWith value idx model =
    { model
        | modalData = Nothing
        , currentParty = model.currentParty |> updatePartyWith value idx
        , searchResults = []
    }


updatePartyWith : Maybe Pokemon -> Int -> PokemonParty -> PokemonParty
updatePartyWith value idx pokemonParty =
    let
        newTotal =
            case value of
                Nothing ->
                    if pokemonParty.total == 0 then
                        0

                    else
                        pokemonParty.total - 1

                Just v ->
                    pokemonParty.total + 1

        newList =
            Array.set idx value pokemonParty.pokemonList
    in
    { pokemonParty | pokemonList = newList, total = newTotal }


findSingleMatch : Model -> String -> List Pokemon -> Maybe Pokemon
findSingleMatch model searchTerm pokemonList =
    case searchTerm |> String.length of
        1 ->
            Nothing

        _ ->
            pokemonList
                |> List.filter
                    (\p -> String.toLower p.name |> String.startsWith (String.toLower searchTerm))
                |> List.filter
                    (\p ->
                        model.currentParty.pokemonList
                            |> Array.toList
                            |> List.member (Just p)
                            |> not
                    )
                |> List.head


findMatchByName : String -> List Pokemon -> List Pokemon
findMatchByName searchTerm pokemonList =
    let
        matchFront =
            pokemonList
                |> List.filter
                    (\p -> String.toLower p.name |> String.startsWith (String.toLower searchTerm))

        matchAnywhere =
            pokemonList
                |> List.filter
                    (\p -> String.toLower p.name |> String.contains (String.toLower searchTerm))
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
    newTypes
        |> calculateDefenses { model | selectedTypes = newTypes }


suggestPokemon : Model -> List Pokemon
suggestPokemon model =
    case model.selectedTypes of
        ( Nothing, Nothing ) ->
            []

        ( Just type1, Nothing ) ->
            model |> allSingleTypeMatches type1

        ( Nothing, Just type1 ) ->
            model |> allSingleTypeMatches type1

        ( Just type1, Just type2 ) ->
            model.allPokemon
                |> List.filter
                    (\pokemon ->
                        case pokemon.pokeType of
                            Single one ->
                                False

                            Dual one two ->
                                (one == type1 && two == type2) || (one == type2 && two == type1)
                    )


suggestFirstPokemon : Model -> Model
suggestFirstPokemon model =
    let
        suggested =
            case model.selectedTypes of
                ( Nothing, Nothing ) ->
                    Nothing

                ( Just type1, Nothing ) ->
                    model |> matchFirstSingleType type1

                ( Nothing, Just type1 ) ->
                    model |> matchFirstSingleType type1

                ( Just type1, Just type2 ) ->
                    model.allPokemon
                        |> List.filter
                            (\pokemon ->
                                case pokemon.pokeType of
                                    Single one ->
                                        False

                                    Dual one two ->
                                        (one == type1 && two == type2) || (one == type2 && two == type1)
                            )
                        |> List.sortWith (\one two -> comparePokemonTypes one.pokeType two.pokeType)
                        |> List.head
    in
    { model | suggestedPokemon = suggested }


allSingleTypeMatches : String -> Model -> List Pokemon
allSingleTypeMatches name model =
    model.allPokemon
        |> List.filter
            (\pokemon ->
                case pokemon.pokeType of
                    Single one ->
                        one == name

                    Dual one two ->
                        one == name || two == name
            )


matchFirstSingleType : String -> Model -> Maybe Pokemon
matchFirstSingleType name model =
    model.allPokemon
        |> List.filter
            (\pokemon ->
                case pokemon.pokeType of
                    Single one ->
                        one == name

                    Dual one two ->
                        one == name || two == name
            )
        |> List.sortWith (\one two -> comparePokemonTypes one.pokeType two.pokeType)
        |> List.head


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
                        [ button [ class "nes-btn", onClick (ChangeMode Pokedex) ] [ text "POKEDEX" ]
                        , button [ class "nes-btn", onClick (ChangeMode Party) ] [ text "PARTY" ]
                        ]
                    , div [ class "btn-container" ]
                        [ button [ class "nes-btn reset-btn", onClick ResetTypeSelections ] [ text "RESET" ] ]
                    , renderTypeList model
                    , renderTypeInfo model
                    ]

                Pokedex ->
                    [ div [ class "btn-container" ]
                        [ button [ class "nes-btn", onClick (ChangeMode TypeInfo) ] [ text "TYPES" ]
                        , button [ class "nes-btn", onClick (ChangeMode Party) ] [ text "PARTY" ]
                        ]
                    , renderPokedex model
                    ]

                Party ->
                    [ div [ class "btn-container" ]
                        [ button [ class "nes-btn", onClick (ChangeMode Pokedex) ] [ text "POKEDEX" ]
                        , button [ class "nes-btn", onClick (ChangeMode TypeInfo) ] [ text "TYPES" ]
                        ]
                    , h1 [] [ text "Party Planner" ]
                    , h4 [ class "grayed" ] [ text "(under construction)" ]
                    , renderPartyMemberModal model
                    , renderPartyGrid model
                    , evaluateParty model
                    ]
    in
    div [ class "container" ]
        renderFn



-- PARTY PLANNER VIEW


renderPartyGrid : Model -> Html Msg
renderPartyGrid model =
    div [ class "party-list" ]
        (model.currentParty.pokemonList
            |> Array.toIndexedList
            |> List.map renderPartyMember
        )


renderPartyMember : ( Int, Maybe Pokemon ) -> Html Msg
renderPartyMember ( idx, member ) =
    case member of
        Nothing ->
            div
                [ class "party-member nes-container is-rounded"
                , onClick (AddPartyMemberAt idx)
                ]
                [ h2 [ class "grayed" ] [ text "Empty" ]
                , h1 [ class "add-btn" ] [ text "+" ]
                ]

        Just pokemon ->
            div
                [ class "party-member nes-container is-rounded"
                , onClick (ClearPartyMemberAt idx)
                ]
                [ h1 [ class "rm-btn" ] [ text "X" ]
                , h2 [ class "member-name" ] [ text pokemon.name ]
                , renderTypeBadgeWithCmd pokemon.pokeType
                ]


renderPartyMemberModal : Model -> Html Msg
renderPartyMemberModal model =
    case model.modalData of
        Nothing ->
            div [] []

        Just modalData ->
            div [ class "party-modal", id "add-pokemon-modal" ]
                [ div [ class "modal-content nes-container is-rounded" ]
                    [ span [ class "close", onClick CloseModal ] [ text "X" ]
                    , h2 [ class "modal-header" ]
                        [ text ("Party Member " ++ String.fromInt (modalData.index + 1)) ]
                    , hr [ class "modal-sep" ] []
                    , div [ class "modal-body" ]
                        [ div [ class "nes-field" ]
                            [ label [ for "search-box" ]
                                [ text "Search Pokemon:" ]
                            , input
                                [ class "nes-input dex-search"
                                , onInput FindPartyMember
                                , type_ "text"
                                , Html.Attributes.value modalData.searchText
                                , autofocus True
                                ]
                                []
                            ]
                        , p [ class "search-hint" ] [ text "Hint:  Click to add to party!" ]
                        , h3 [] (List.map (renderPartySearchResults modalData.index) model.searchResults)
                        ]
                    ]
                ]


renderPartySearchResults : Int -> Pokemon -> Html Msg
renderPartySearchResults idx pokemon =
    li [ class "search-result-item", onClick (ConfirmPartyMember idx (Just pokemon)) ]
        [ strong [] [ text pokemon.name ]
        , div
            [ class "type-link" ]
            [ pokemon.pokeType |> renderTypeBadge ]
        ]


evaluateParty : Model -> Html Msg
evaluateParty model =
    let
        content =
            case model.currentParty.total of
                0 ->
                    p [] []

                6 ->
                    evaluateFullParty model

                5 ->
                    suggestPartyMember model

                _ ->
                    typeCoverageForParty model
    in
    div [ class "eval-container nes-container is-rounded with-title" ]
        [ p [ class "title" ] [ text "Evaluation" ]
        , content
        ]


typeCoverageForParty : Model -> Html Msg
typeCoverageForParty model =
    -- List of types not covered yet
    let
        typeCoverageGaps =
            model.currentParty
                |> evaluateTypeCoverage model.allTypes
                |> List.map (\t -> t.name)
    in
    case typeCoverageGaps of
        [] ->
            div [ class "type-coverage" ]
                [ h3 [] [ text "You have every type matchup covered!" ] ]

        typeGaps ->
            div [ class "type-coverage" ]
                [ h3 [] [ text "You have no pokemon strong against:" ]
                , typeGaps |> renderBadgeListWithCmd
                ]


suggestPartyMember : Model -> Html Msg
suggestPartyMember model =
    let
        typeCoverageGaps =
            model.currentParty |> evaluateTypeCoverage model.allTypes

        possibleTypes =
            case typeCoverageGaps of
                [] ->
                    []

                typeGaps ->
                    typeGaps
                        |> getStrengthsOfTypes model
                        |> List.map
                            (\curType ->
                                div
                                    [ class "type-link btm-gap"
                                    , onClick (SetType curType)
                                    ]
                                    [ curType |> renderTypeBadgeWithCmd ]
                            )
    in
    case possibleTypes of
        [] ->
            div [] [ typeCoverageForParty model ]

        types ->
            div []
                [ typeCoverageForParty model
                , h3 [] [ text "Type suggestions for last pokemon:" ]
                , div [ class "badge-container" ] types
                ]


evaluateFullParty : Model -> Html Msg
evaluateFullParty model =
    div []
        [ typeCoverageForParty model
        , highPartyWeakness model
        , overlappedCoverage model
        ]


evaluateTypeCoverage : List Type -> PokemonParty -> List Type
evaluateTypeCoverage typeList party =
    -- For a given PokemonParty, evaluate what types you aren't 2x damage against
    typeList
        |> List.filter
            (\curType ->
                party.pokemonList
                    |> Array.toList
                    |> List.filterMap identity
                    |> List.any (pokemonStrongAgainst curType typeList)
                    |> not
            )


pokemonStrongAgainst : Type -> List Type -> Pokemon -> Bool
pokemonStrongAgainst checkType typeList pokemon =
    typeList
        |> List.filter
            (\t ->
                case pokemon.pokeType of
                    Single t1 ->
                        t.name == t1

                    Dual t1 t2 ->
                        t.name == t1 || t.name == t2
            )
        |> List.any (\t -> t.strengths |> List.member checkType.name)


getStrengthsOfTypes : Model -> List Type -> List PokemonType
getStrengthsOfTypes model typeGaps =
    model.allPokemon
        |> List.map .pokeType
        -- Filter only to types that exist in a pokemon
        |> LX.uniqueBy
            (\checkType ->
                case checkType of
                    Single t1 ->
                        t1

                    Dual t1 t2 ->
                        [ t1, t2 ] |> List.sort |> String.join "/"
            )
        -- Now lets find the best type combo (or just type!)
        |> buildAllTypesStrengthLists model.allTypes
        |> List.map
            (\( pokeType, strengths ) ->
                ( pokeType
                , strengths
                    |> List.filter
                        (\st ->
                            typeGaps
                                |> List.map .name
                                |> List.member st
                        )
                )
            )
        -- Sort by what type(s) have the highest coverage of missing types, then by Single over Dual
        |> List.sortWith comparePokemonTypeStrengths
        -- Sort goes backwards from what I actually want...
        |> List.reverse
        |> List.map Tuple.first
        |> List.take 5


comparePokemonTypeStrengths : ( PokemonType, List String ) -> ( PokemonType, List String ) -> Order
comparePokemonTypeStrengths ( type1, strengths1 ) ( type2, strengths2 ) =
    case compare (List.length strengths1) (List.length strengths2) of
        GT ->
            GT

        LT ->
            LT

        EQ ->
            comparePokemonTypes type1 type2


comparePokemonTypes : PokemonType -> PokemonType -> Order
comparePokemonTypes type1 type2 =
    case ( type1, type2 ) of
        ( Dual _ _, Single _ ) ->
            LT

        ( Single _, Dual _ _ ) ->
            GT

        _ ->
            EQ


buildAllTypesStrengthLists : List Type -> List PokemonType -> List ( PokemonType, List String )
buildAllTypesStrengthLists allTypes existingTypes =
    existingTypes
        |> LX.uniqueBy
            (\pokeType ->
                case pokeType of
                    Single t1 ->
                        t1 |> List.singleton

                    Dual t1 t2 ->
                        [ t1, t2 ] |> List.sort
            )
        |> List.map
            (\pokeType -> ( pokeType, pokeType |> buildStrengths allTypes ))


buildStrengths : List Type -> PokemonType -> List String
buildStrengths allTypes pokeType =
    let
        typeList =
            case pokeType of
                Single t1 ->
                    t1 |> List.singleton

                Dual t1 t2 ->
                    [ t1, t2 ]
    in
    typeList
        |> List.map (typeNameToType allTypes)
        |> List.map .strengths
        |> List.concat
        |> LX.unique


typeNameToType : List Type -> String -> Type
typeNameToType allTypes typeName =
    allTypes
        |> List.filter (\t -> t.name == typeName)
        |> List.head
        |> Maybe.withDefault { name = "", strengths = [], weaknesses = [], ineffectives = [], noEffects = [] }


highPartyWeakness : Model -> Html Msg
highPartyWeakness model =
    div [ class "high-weakness" ]
        [ h3 [] [ text "3+ Pokemon are weak against these types:" ]
        , model
            |> overlappedAttributeCoverage buildTypeWeaknessList
            |> renderBadgeListWithCmd
        ]


buildTypeWeaknessList : Model -> PokemonType -> List String
buildTypeWeaknessList model pokemonType =
    pokemonType
        |> buildTypeList model
        |> List.map .weaknesses
        |> List.concat
        |> LX.unique


buildTypeStrengthList : Model -> PokemonType -> List String
buildTypeStrengthList model pokemonType =
    pokemonType
        |> buildTypeList model
        |> List.map .strengths
        |> List.concat
        |> LX.unique


buildTypeList : Model -> PokemonType -> List Type
buildTypeList model pokemonType =
    let
        typeList =
            case pokemonType of
                Single type1 ->
                    type1 |> List.singleton

                Dual type1 type2 ->
                    [ type1, type2 ]
    in
    typeList |> List.map (typeNameToType model.allTypes)


overlappedAttributeCoverage : (Model -> PokemonType -> List String) -> Model -> List String
overlappedAttributeCoverage attrFn model =
    let
        partyAttrs =
            model.currentParty.pokemonList
                |> Array.toList
                |> List.filterMap identity
                |> List.map (\pokemon -> attrFn model pokemon.pokeType)
    in
    model.allTypes
        |> List.map
            (\curType ->
                ( curType.name
                , partyAttrs
                    |> List.filter (List.member curType.name)
                    |> List.length
                )
            )
        |> List.filter
            (\( _, weakCount ) ->
                weakCount >= 3
            )
        |> List.map Tuple.first


overlappedCoverage : Model -> Html Msg
overlappedCoverage model =
    div [ class "high-weakness" ]
        [ h3 [] [ text "3+ Pokemon are strong against these types:" ]
        , model
            |> overlappedAttributeCoverage buildTypeStrengthList
            |> renderBadgeListWithCmd
        ]



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
            div []
                [ div [ class "nes-container with-title is-rounded" ]
                    [ p [ class "title" ] [ text ("Dual Type: " ++ one ++ "/" ++ two) ]
                    , renderDefenses model
                    ]
                , renderSuggestedPokemon model
                ]

        ( Just typeName, Nothing ) ->
            div []
                [ typeName |> renderSingleTypeInfo model
                , renderSuggestedPokemon model
                ]

        ( Nothing, Just typeName ) ->
            div []
                [ typeName |> renderSingleTypeInfo model
                , renderSuggestedPokemon model
                ]

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


renderSuggestedPokemon : Model -> Html Msg
renderSuggestedPokemon model =
    let
        innerHtml =
            case model.suggestedPokemon of
                Nothing ->
                    h3 [] [ text "No Pokemon exist with this type" ]

                Just pokemon ->
                    div [ onClick RandomizeSuggestion ]
                        [ h3 [ class "inline" ] [ text pokemon.name ]
                        , h4 [ class "grayed inline" ] [ text "  (click to shuffle)" ]
                        ]
    in
    div [ class "nes-container with-title is-rounded" ]
        [ p [ class "title" ] [ text "Suggested Pokemon" ]
        , innerHtml
        ]



-- POKEDEX VIEW


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


renderBadgeListWithCmd : List String -> Html Msg
renderBadgeListWithCmd typeList =
    div [ class "badge-container" ]
        (typeList
            |> List.map (\i -> renderTypeBadgeWithCmd (Single i))
        )


renderTypeBadgeWithCmd : PokemonType -> Html Msg
renderTypeBadgeWithCmd pokeType =
    case pokeType of
        Single t ->
            a [ class "nes-badge type-badge", onClick (SetType pokeType) ]
                [ span
                    [ class (String.toLower t ++ "-badge") ]
                    [ text t ]
                ]

        Dual one two ->
            -- TODO:  Follow example split badge to make left/right versions of each type badge!
            a [ class "nes-badge type-badge", onClick (SetType pokeType) ]
                [ span [ class (String.toLower one ++ "-badge") ]
                    [ text one ]
                , span [ class (String.toLower two ++ "-badge") ]
                    [ text two ]
                ]


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
            a [ class "nes-badge type-badge" ]
                [ span
                    [ class (String.toLower t ++ "-badge") ]
                    [ text t ]
                ]

        Dual one two ->
            -- TODO:  Follow example split badge to make left/right versions of each type badge!
            a [ class "nes-badge type-badge" ]
                [ span [ class (String.toLower one ++ "-badge") ]
                    [ text one ]
                , span [ class (String.toLower two ++ "-badge") ]
                    [ text two ]
                ]
