module Main exposing (main)

import Array exposing (get, repeat, set)
import Browser
import Browser.Dom as Dom exposing (focus)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error, expectJson, get)
import Json.Decode as JD exposing (..)
import List.Extra as LX exposing (gatherEqualsBy, groupsOf, uniqueBy)
import Pokemon exposing (..)
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
        , subscriptions = \_ -> Sub.none
        }



-- MODELS/TYPES


type alias ModalData =
    { searchText : String
    , index : Int
    }


type Mode
    = TypeInfo
    | Pokedex
    | Party


type alias Model =
    { error : Maybe String
    , mode : Mode
    , allTypes : List Type
    , typeMap : Dict String Type
    , allPokemon : List Pokemon
    , searchResults : List Pokemon
    , selectedTypes : ( Maybe String, Maybe String )
    , examplePokemon : Maybe Pokemon
    , currentTypeDefenses : Defenses
    , currentTypeOffenses : Offenses
    , currentParty : PokemonParty
    , modalData : Maybe ModalData
    }



-- Loading Data


decodeTypeData : JD.Decoder Type
decodeTypeData =
    JD.map7 Type
        (JD.field "name" JD.string)
        (JD.field "strengths" (JD.list JD.string))
        (JD.field "weaknesses" (JD.list JD.string))
        (JD.field "ineffectives" (JD.list JD.string))
        (JD.field "no_effects" (JD.list JD.string))
        (JD.field "defenses" (JD.list JD.string))
        (JD.field "immune" (JD.list JD.string))


decodePokemonData : Dict String Type -> JD.Decoder Pokemon
decodePokemonData typeDict =
    JD.map4 Pokemon
        (JD.field "name" JD.string)
        (JD.field "number" JD.string)
        (JD.field "pokeType"
            (JD.string
                |> JD.andThen
                    (\typeStr ->
                        case split "/" typeStr of
                            [ typeOne, typeTwo ] ->
                                case ( Dict.get typeOne typeDict, Dict.get typeTwo typeDict ) of
                                    ( Just a, Just b ) ->
                                        JD.succeed (Dual a b)

                                    _ ->
                                        JD.fail ("Invalid Type in " ++ typeOne ++ " " ++ typeTwo)

                            [ typeName ] ->
                                case Dict.get typeName typeDict of
                                    Just a ->
                                        JD.succeed (Single a)

                                    Nothing ->
                                        JD.fail ("Invalid Type in " ++ typeStr)

                            _ ->
                                JD.fail ("Invalid PokemonType " ++ typeStr)
                    )
            )
        )
        (JD.field "exclusive" JD.string)


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
    { error = Nothing
    , mode = Pokedex
    , allTypes = []
    , typeMap = Dict.empty
    , allPokemon = []
    , searchResults = []
    , selectedTypes = ( Nothing, Nothing )
    , examplePokemon = Nothing
    , currentTypeDefenses = Pokemon.initDefenses
    , currentTypeOffenses = Pokemon.initOffenses
    , currentParty = Pokemon.initParty
    , modalData = Nothing
    }



-- UPDATE


type Msg
    = NoOp
    | LoadData
    | TypesLoaded (Result Http.Error (List Type))
    | PokemonLoaded (Result Http.Error (List Pokemon))
    | ChangeMode Mode
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
            , getDataList "poketypes" decodeTypeData TypesLoaded
            )

        TypesLoaded result ->
            case result of
                Err httpError ->
                    case httpError of
                        Http.BadBody errStr ->
                            ( { model | error = Just errStr }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Ok typeData ->
                    let
                        typeMap =
                            typeData
                                |> List.map (\td -> ( td.name, td ))
                                |> Dict.fromList
                    in
                    ( { model
                        | typeMap = typeMap
                        , allTypes = typeData
                      }
                    , getDataList "pokedex_sv" (decodePokemonData typeMap) PokemonLoaded
                    )

        PokemonLoaded result ->
            case result of
                Err httpError ->
                    case httpError of
                        Http.BadBody errStr ->
                            ( { model | error = Just errStr }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Ok pokeData ->
                    ( { model | allPokemon = pokeData }, Cmd.none )

        ChangeMode mode ->
            ( { model | mode = mode, searchResults = [] }, Cmd.none )

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
            ( model |> updateSelectedTypes typeName |> suggestFirstPokemon, Cmd.none )

        SetType pokeType ->
            let
                ( newModel, nextType ) =
                    case pokeType of
                        Single t ->
                            ( { model | selectedTypes = ( Nothing, Nothing ) }, t.name )

                        Dual one two ->
                            ( { model | selectedTypes = ( Just one.name, Nothing ) }, two.name )
            in
            ( { newModel | mode = TypeInfo }
            , nextType
                |> Task.succeed
                |> Task.perform SelectType
            )

        ResetTypeSelections ->
            ( { model | selectedTypes = ( Nothing, Nothing ) }, Cmd.none )

        AddPartyMemberAt idx ->
            ( { model
                | modalData = Just { index = idx, searchText = "" }
                , searchResults = []
              }
            , Dom.focus "addMemberInput"
                |> Task.attempt (\_ -> NoOp)
            )

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
            let
                examplePokemonList =
                    case model.examplePokemon of
                        Nothing ->
                            getExamplePokemon model

                        Just pokemon ->
                            if List.length (getExamplePokemon model) > 1 then
                                getExamplePokemon model |> LX.remove pokemon

                            else
                                -- If its the only pokemon, keep suggesting it
                                pokemon |> List.singleton
            in
            ( model, Random.generate NewSuggestion (RandList.choose examplePokemonList) )

        NewSuggestion ( pokemon, _ ) ->
            ( { model | examplePokemon = pokemon }, Cmd.none )



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


findMatchByName : String -> List Pokemon -> List Pokemon
findMatchByName searchTerm pokemonList =
    case String.length searchTerm of
        1 ->
            pokemonList |> Pokemon.findByName searchTerm

        _ ->
            pokemonList |> Pokemon.fuzzyFindByName searchTerm


updateSelectedTypes : String -> Model -> Model
updateSelectedTypes newType model =
    let
        newTypes =
            case model.selectedTypes of
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
        |> calculateDualTypeStats { model | selectedTypes = newTypes }


getExamplePokemon : Model -> List Pokemon
getExamplePokemon model =
    model.allPokemon
        |> List.filter (Pokemon.pokemonMatchesTypes model.selectedTypes)


suggestFirstPokemon : Model -> Model
suggestFirstPokemon model =
    let
        example =
            model
                |> getExamplePokemon
                |> List.sortWith (\one two -> Pokemon.comparePokemonTypes one.pokeType two.pokeType)
                |> List.head
    in
    { model | examplePokemon = example }


matchFirstSingleType : String -> Model -> Maybe Pokemon
matchFirstSingleType name model =
    model.allPokemon
        |> List.filter
            (\pokemon ->
                case pokemon.pokeType of
                    Single one ->
                        one.name == name

                    Dual one two ->
                        one.name == name || two.name == name
            )
        |> List.sortWith (\one two -> Pokemon.comparePokemonTypes one.pokeType two.pokeType)
        |> List.head


getBoolValue : List String -> String -> Int
getBoolValue typeList typeName =
    -- Value is used for calculating "rating" of a Type matchup in `calculateDualTypeStats`
    if typeList |> List.member typeName then
        1

    else
        0


assignByValue : ( Int, List String ) -> Defenses -> Defenses
assignByValue ( val, typeList ) defenses =
    {- Depending on the "Rating" computed by `calculateDualTypeStats`,
       Add the data to a specific part of the Defenses
    -}
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


getOffenses : Model -> String -> List String
getOffenses model pokemonType =
    model.allTypes
        |> List.filter (\t -> t.name == pokemonType)
        |> List.map (\t -> t.strengths)
        |> List.concat
        |> LX.unique


getIneffectives : Model -> String -> List String
getIneffectives model pokemonType =
    model.allTypes
        |> List.filter (\t -> t.name == pokemonType)
        |> List.map (\t -> t.ineffectives)
        |> List.concat
        |> LX.unique


calculateDualTypeStats : Model -> ( Maybe String, Maybe String ) -> Model
calculateDualTypeStats model bothTypes =
    {- This is a doozy.  It calculates the offensive and defensive matchup for
       a Pokemon. The tricky part is that for dual types there's additive
       weakness and even cancelling weaknesses with strengths.

       OFFENSES:
       This function simply creates a uniqued set of all "strengths" each type
       in the dual type has, and shares that as the types a pokemon with this
       dual-type will have.

       DEFENSES:
       This function calculates a "Rating" for each existing type against
       the given type.  This rating changes based on strengths/weaknesses
       of the two types in conjunction with each other.

       Example:  Bug is weak to Fire (x2 damage), but Fire is "ineffective"
       against Water.  So a Bug/Water pokemon will cancel its "Fire weakness"
       with a "Fire strength", resulting in Fire not being added (since the
       pokemon takes normal damage from Fire)

       Example 2:  Bug is weak to Fire, and Steel is ALSO weak to Fire.  So a
       Bug/Steel pokemon will take x4 damage from Fire, resulting in a rating
       reflecting that.
    -}
    let
        dualType =
            bothTypes |> Tuple.mapBoth (Maybe.withDefault "") (Maybe.withDefault "")

        doubles =
            ( dualType |> Tuple.first |> getOffenses model
            , dualType |> Tuple.second |> getOffenses model
            )

        halves =
            ( dualType |> Tuple.first |> getIneffectives model
            , dualType |> Tuple.second |> getIneffectives model
            )

        sortedMap =
            model.allTypes
                |> List.map
                    (\curType ->
                        let
                            -- Rating for whether curType is strong against 1 or both types
                            -- (Will result in a value of 0-2)
                            bad =
                                dualType
                                    |> Tuple.mapBoth (getBoolValue curType.strengths) (getBoolValue curType.strengths)
                                    |> TX.uncurry (+)

                            -- Rating for whether curType is ineffective against 1 or both types
                            -- (Will result in a value of 0-2)
                            good =
                                dualType
                                    |> Tuple.mapBoth (getBoolValue curType.ineffectives) (getBoolValue curType.ineffectives)
                                    |> TX.uncurry (+)

                            -- Check if the curType has "no effect" on either of the types
                            -- If this is true, then everything else is cancelled out.
                            noEffect =
                                dualType
                                    |> Tuple.mapBoth (\a -> List.member a curType.noEffects) (\a -> List.member a curType.noEffects)
                                    |> TX.uncurry (||)
                        in
                        if noEffect then
                            -- Cancel out the rating, curType can't hurt this type combo
                            ( curType.name, 5 )

                        else
                            -- Take the good rating and cancel out with the bad.
                            -- Add 2 so the value is kept in the position
                            -- (Will result in a value of 0-4)
                            ( curType.name, good - bad + 2 )
                    )
                -- Gather together the similarly-rated types
                |> LX.gatherEqualsBy Tuple.second
                -- We now have List ((String, Int), List (String, Int))
                |> List.map
                    -- Each valueSet is a ((name, rating), List (name, rating))
                    (\valueSet ->
                        Tuple.pair
                            (Tuple.second
                                (Tuple.first valueSet)
                            )
                            ([ Tuple.first (Tuple.first valueSet) ]
                                ++ List.map (\a -> Tuple.first a) (Tuple.second valueSet)
                            )
                    )
                -- Results in a list of each rating along with the list of types with that rating
                |> List.sortBy Tuple.first
    in
    { model
        | currentTypeDefenses =
            sortedMap
                -- Create the Defense object using the ratings to determine where things go
                |> List.foldl assignByValue (Defenses [] [] [] [] [])
        , currentTypeOffenses = { x2 = doubles, half = halves, none = [] }
    }



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
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
                    , renderPartyMemberModal model
                    , renderPartyGrid model
                    , evaluateParty model
                    ]
    in
    div [ class "container" ]
        (case model.error of
            Nothing ->
                content

            Just error ->
                [ renderErrorSection error ]
        )


renderErrorSection : String -> Html Msg
renderErrorSection errorStr =
    div [ class "nes-container is-rounded with-title" ]
        [ span [ class "title" ]
            [ a [ class "nes-badge" ]
                [ span [ class "is-error" ] [ text "ERROR" ] ]
            ]
        , h2 [] [ text "Something went wrong!" ]
        , h3 [ class "error-txt" ] [ text errorStr ]
        , h3 []
            [ text "Please report an issue at my "
            , a [ href "https://github.com/3digitdev/swshdex" ]
                [ text "GitHub Repo ", i [ class "nes-icon github is-medium" ] [] ]
            ]
        ]



-- PARTY PLANNER VIEW
-- Party Planner -> Party Section


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
                [ h2 [ class "grayed inline top-gap" ] [ text "Empty" ]
                , h1 [ class "party-btn add-btn" ] [ text "+" ]
                ]

        Just pokemon ->
            div
                [ class "party-member nes-container is-rounded"
                , onClick (ClearPartyMemberAt idx)
                ]
                [ h1 [ class "party-btn rm-btn" ] [ text "X" ]
                , h2 [ class "member-name" ] [ text pokemon.name ]
                , renderTypeBadgeWithCmd pokemon.pokeType
                ]



-- PartyPlanner -> Modal


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
                                , id "addMemberInput"
                                , onInput FindPartyMember
                                , type_ "text"
                                , Html.Attributes.value modalData.searchText
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



-- Party Planner -> Party Evaluation Section


evaluateParty : Model -> Html Msg
evaluateParty model =
    let
        content =
            case model.currentParty.total of
                0 ->
                    []

                6 ->
                    evaluateFullParty model

                5 ->
                    suggestPartyMember model

                _ ->
                    typeCoverageForParty model
    in
    div [ class "eval-container nes-container is-rounded with-title" ]
        ([ p [ class "title" ] [ text "Evaluation" ] ] ++ content)


evaluateFullParty : Model -> List (Html Msg)
evaluateFullParty model =
    List.concat
        [ typeCoverageForParty model
        , model |> highAttributeOverlap Pokemon.buildWeaknessList "weak"
        , model |> highAttributeOverlap Pokemon.buildStrengthList "strong"
        ]


highAttributeOverlap : (PokemonType -> List String) -> String -> Model -> List (Html Msg)
highAttributeOverlap buildAttrListFn attrStr model =
    case overlappedAttributeCoverage buildAttrListFn model of
        [] ->
            []

        typeList ->
            [ h3 [] [ text ("3+ Pokemon are " ++ attrStr ++ " against these types:") ]
            , renderBadgeListWithCmd typeList
            ]


overlappedAttributeCoverage : (PokemonType -> List String) -> Model -> List Type
overlappedAttributeCoverage attrFn model =
    let
        partyAttrs =
            model.currentParty.pokemonList
                |> Array.toList
                |> List.filterMap identity
                |> List.map (\pokemon -> attrFn pokemon.pokeType)
    in
    model.allTypes
        |> List.filter
            (\curType ->
                partyAttrs
                    |> List.filter (List.member curType.name)
                    |> List.length
                    -- confusing! filters for lists of length >= 3
                    |> (<) 3
            )


suggestPartyMember : Model -> List (Html Msg)
suggestPartyMember model =
    case Pokemon.evaluateTypeCoverage model.allTypes model.currentParty of
        [] ->
            typeCoverageForParty model

        typeGaps ->
            typeCoverageForParty model
                ++ [ h3 [] [ text "Type suggestions for last pokemon:" ]
                   , div [ class "badge-container" ]
                        (typeGaps
                            |> getStrengthsOfTypes model
                            |> List.map
                                (\curType ->
                                    div [ class "type-link-no-indent btm-gap", onClick (SetType curType) ]
                                        [ curType |> renderTypeBadgeWithCmd ]
                                )
                        )
                   ]


getStrengthsOfTypes : Model -> List Type -> List PokemonType
getStrengthsOfTypes model typeGaps =
    model.allPokemon
        |> List.map .pokeType
        -- Filter only to types that exist in a pokemon
        |> LX.uniqueBy
            (\checkType ->
                checkType
                    |> Pokemon.typeAsList
                    |> List.map .name
                    |> List.sort
                    |> String.join "/"
            )
        -- Now lets find the best type combo (or just type!)
        |> Pokemon.buildAllTypesStrengthLists model.allTypes
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
        |> List.sortWith Pokemon.comparePokemonTypeStrengths
        -- Sort goes backwards from what I actually want...
        |> List.reverse
        |> List.map Tuple.first
        |> List.take 5


typeCoverageForParty : Model -> List (Html Msg)
typeCoverageForParty model =
    -- List of types not covered yet
    case Pokemon.evaluateTypeCoverage model.allTypes model.currentParty of
        [] ->
            [ h3 []
                [ i [ class "nes-icon is-medium star" ] []
                , text "You have every type matchup covered!"
                , i [ class "nes-icon is-medium star" ] []
                ]
            ]

        typeGaps ->
            [ h3 [] [ text "You have no pokemon strong against:" ]
            , typeGaps |> renderBadgeListWithCmd
            ]



-- TYPE MATCHUP VIEW


renderTypeList : Model -> Html Msg
renderTypeList model =
    case model.allTypes of
        [] ->
            div [ class "nes-container is-rounded with-title" ]
                [ p [ class "title" ]
                    [ text "ERROR" ]
                , h2 [] [ text "Something went wrong loading Types JSON!" ]
                ]

        typeData ->
            div [ class "type-table nes-table-responsive" ]
                [ table [ class "nes-table is-bordered is-centered" ]
                    (typeData
                        |> LX.groupsOf 2
                        |> List.map
                            (\two ->
                                tr []
                                    (List.map
                                        (\one ->
                                            model |> renderTypeButton one.name
                                        )
                                        two
                                    )
                            )
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
                    , renderDualTypeInfo model
                    ]
                , renderExamplePokemon model
                ]

        ( Just typeName, Nothing ) ->
            div []
                [ typeName |> renderSingleTypeInfo model
                , renderExamplePokemon model
                ]

        ( Nothing, Just typeName ) ->
            div []
                [ typeName |> renderSingleTypeInfo model
                , renderExamplePokemon model
                ]

        _ ->
            div [] []


renderDualTypeInfo : Model -> Html Msg
renderDualTypeInfo model =
    let
        immunities =
            case model.currentTypeDefenses.x0 |> List.length of
                0 ->
                    h3 [] [ strong [] [ text "None" ] ]

                _ ->
                    model.currentTypeDefenses.x0 |> renderDefenseInfoSet model "No"

        ( typeOne, typeTwo ) =
            model.selectedTypes |> Tuple.mapBoth (Maybe.withDefault "") (Maybe.withDefault "")
    in
    div []
        [ h3 [] [ text "Offense:" ]
        , h4 [ class ((typeOne |> String.toLower) ++ "-color type-label") ] [ text typeOne ]
        , model.currentTypeOffenses.x2 |> Tuple.first |> renderOffenseInfoSet model "2x"
        , model.currentTypeOffenses.half |> Tuple.first |> renderOffenseInfoSet model "1/2"
        , h4 [ class ((typeTwo |> String.toLower) ++ "-color type-label") ] [ text typeTwo ]
        , model.currentTypeOffenses.x2 |> Tuple.second |> renderOffenseInfoSet model "2x"
        , model.currentTypeOffenses.half |> Tuple.second |> renderOffenseInfoSet model "1/2"
        , hr [] []
        , h3 [] [ text "Defense:" ]
        , model.currentTypeDefenses.x4 |> renderDefenseInfoSet model "4x"
        , model.currentTypeDefenses.x2 |> renderDefenseInfoSet model "2x"
        , model.currentTypeDefenses.half |> renderDefenseInfoSet model "1/2"
        , model.currentTypeDefenses.quarter |> renderDefenseInfoSet model "1/4"
        , immunities
        ]


renderDefenseInfoSet : Model -> String -> List String -> Html Msg
renderDefenseInfoSet model modifier typeNameList =
    model
        |> renderIfNotEmptyTypeList typeNameList
            (\types ->
                p []
                    [ strong [] [ text (modifier ++ " damage from:") ]
                    , renderBadgeList types
                    ]
            )


renderOffenseInfoSet : Model -> String -> List String -> Html Msg
renderOffenseInfoSet model modifier typeNameList =
    model
        |> renderIfNotEmptyTypeList typeNameList
            (\types ->
                p []
                    [ strong [] [ text (modifier ++ " damage against:") ]
                    , renderBadgeList types
                    ]
            )


renderSingleTypeInfo : Model -> String -> Html Msg
renderSingleTypeInfo model typeName =
    case Pokemon.namesToTypes model.typeMap [ typeName ] of
        [ singleType ] ->
            div [ class "nes-container with-title is-rounded" ]
                [ p [ class "title" ] [ text (typeName ++ " Type") ]
                , h3 [] [ text "Offense:" ]
                , singleType.strengths |> renderSingleInfoSet model "2x damage to:"
                , singleType.ineffectives |> renderSingleInfoSet model "1/2 damage to:"
                , singleType.noEffects |> renderSingleInfoSet model "Does not affect:"
                , hr [] []
                , h3 [] [ text "Defense:" ]
                , singleType.weaknesses |> renderSingleInfoSet model "2x damage from:"
                , singleType.defenses |> renderSingleInfoSet model "1/2 damage from:"
                , singleType.immune |> renderSingleInfoSet model "Immune to:"
                ]

        _ ->
            div [] []


renderSingleInfoSet : Model -> String -> List String -> Html Msg
renderSingleInfoSet model preText typeNameList =
    model
        |> renderIfNotEmptyTypeList typeNameList
            (\types ->
                div []
                    [ h4 [] [ text preText ]
                    , renderBadgeList types
                    ]
            )


renderIfNotEmptyTypeList : List String -> (List Type -> Html Msg) -> Model -> Html Msg
renderIfNotEmptyTypeList typeNameList renderFn model =
    case Pokemon.namesToTypes model.typeMap typeNameList of
        [] ->
            div [] []

        types ->
            renderFn types


renderExamplePokemon : Model -> Html Msg
renderExamplePokemon model =
    let
        innerHtml =
            case model.examplePokemon of
                Nothing ->
                    h3 [] [ text "No Pokemon exist with this type" ]

                Just pokemon ->
                    div [ onClick RandomizeSuggestion ]
                        [ h4 []
                            [ text
                                ("Total Pokemon: "
                                    ++ (model
                                            |> getExamplePokemon
                                            |> List.length
                                            |> String.fromInt
                                       )
                                )
                            ]
                        , h3 [ class "inline" ] [ text pokemon.name ]
                        , h4 [ class "grayed inline" ] [ text "  (click to shuffle)" ]
                        ]
    in
    div [ class "nes-container with-title is-rounded" ]
        [ p [ class "title" ] [ text "Example Pokemon" ]
        , innerHtml
        ]



-- POKEDEX VIEW


renderPokedex : Model -> Html Msg
renderPokedex model =
    div []
        [ h1 [] [ text "Pokemon" ]

        -- , p [ class "search-hint" ] [ text "Now updated for Isle of Armor/Crown Tundra!" ]
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

        -- , p [ class "search-hint" ] [ text "Legend:  Purple is Galarian variant, Blue is Alolan" ]
        , ul [] (List.map renderPokemon model.searchResults)
        ]


renderPokemon : Pokemon -> Html Msg
renderPokemon pokemon =
    let
        dexNum =
            if pokemon.number == "" then
                "N/A: "

            else
                pokemon.number ++ ": "

        -- armorDexNum =
        --     if pokemon.armorNum == "" then
        --         span [] []
        --     else
        --         span [ class "expac-num" ]
        --             [ img [ class "expac-icon", src "armor.png" ] []
        --             , text pokemon.armorNum
        --             ]
        -- crownDexNum =
        --     if pokemon.crownNum == "" then
        --         span [] []
        --     else
        --         span [ class "expac-num" ]
        --             [ img [ class "expac-icon", src "crown.png" ] []
        --             , text pokemon.crownNum
        --             ]
        exclusiveIcon =
            case pokemon.exclusive of
                "Shield" ->
                    span [] [ img [ class "exclusive-icon", src "shield.png" ] [] ]

                "Sword" ->
                    span [] [ img [ class "exclusive-icon", src "sword.png" ] [] ]

                "Scarlet" ->
                    span [] [ img [ class "exclusive-icon", src "scarlet.svg" ] [] ]

                "Violet" ->
                    span [] [ img [ class "exclusive-icon", src "violet.svg" ] [] ]

                _ ->
                    span [ class "no-exclusive" ] [ text "" ]

        name =
            if String.contains "Galarian" pokemon.name then
                strong [ class "galar-name" ] [ text (pokemon.name |> String.replace "Galarian " "") ]

            else if String.contains "Alolan" pokemon.name then
                strong [ class "alola-name" ] [ text (pokemon.name |> String.replace "Alolan " "") ]

            else
                strong [] [ text pokemon.name ]
    in
    li [ class "search-result-item" ]
        [ text dexNum
        , name
        , exclusiveIcon

        -- , armorDexNum
        -- , crownDexNum
        , div
            [ class "type-link"
            , onClick (SetType pokemon.pokeType)
            ]
            [ pokemon.pokeType |> renderTypeBadge ]
        ]



-- Rendering Type Badges


renderBadgeListWithCmd : List Type -> Html Msg
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
                    [ class (String.toLower t.name ++ "-badge") ]
                    [ text t.name ]
                ]

        Dual one two ->
            -- TODO:  Follow example split badge to make left/right versions of each type badge!
            a [ class "nes-badge is-splited type-badge", onClick (SetType pokeType) ]
                [ span [ class (String.toLower one.name ++ "-badge-left dual-left") ]
                    [ text one.name ]
                , span [ class (String.toLower two.name ++ "-badge-right") ]
                    [ text two.name ]
                ]


renderBadgeList : List Type -> Html Msg
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
                    [ class (String.toLower t.name ++ "-badge") ]
                    [ text t.name ]
                ]

        Dual one two ->
            a [ class "nes-badge is-splited type-badge" ]
                [ span [ class (String.toLower one.name ++ "-badge-left dual-left") ]
                    [ text one.name ]
                , span [ class (String.toLower two.name ++ "-badge-right") ]
                    [ text two.name ]
                ]
