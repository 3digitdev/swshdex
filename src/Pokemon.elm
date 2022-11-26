module Pokemon exposing
    ( Defenses
    , Offenses
    , Pokemon
    , PokemonParty
    , PokemonType(..)
    , Type
    , buildAllTypesStrengthLists
    , buildStrengthList
    , buildWeaknessList
    , comparePokemonTypeStrengths
    , comparePokemonTypes
    , evaluateTypeCoverage
    , findByName
    , fuzzyFindByName
    , initDefenses
    , initOffenses
    , initParty
    , namesToTypes
    , pokemonMatchesTypes
    , typeAsList
    )

import Array exposing (Array)
import Dict exposing (Dict)
import List
import List.Extra as LX



-- PokemonParty


type alias PokemonParty =
    { pokemonList : Array.Array (Maybe Pokemon)
    , total : Int
    , suggestedPokemon : Maybe Pokemon
    }


initParty : PokemonParty
initParty =
    { pokemonList = Array.repeat 6 Nothing
    , total = 0
    , suggestedPokemon = Nothing
    }


evaluateTypeCoverage : List Type -> PokemonParty -> List Type
evaluateTypeCoverage typeList party =
    -- For a given PokemonParty, evaluate what types you aren't 2x damage against
    typeList
        |> List.filter
            (\curType ->
                party.pokemonList
                    |> Array.toList
                    |> List.filterMap identity
                    |> List.any (isStrongAgainst curType typeList)
                    |> not
            )



-- Pokemon


type alias Pokemon =
    { name : String
    , number : String
    , pokeType : PokemonType
    -- , armorNum : String
    -- , crownNum : String
    , exclusive : String
    }


findWithMethod : String -> (String -> String -> Bool) -> List Pokemon -> List Pokemon
findWithMethod searchTerm searchFn pokemonList =
    pokemonList
        |> List.filter
            (\p -> String.toLower p.name |> searchFn (String.toLower searchTerm))


findByName : String -> List Pokemon -> List Pokemon
findByName searchTerm pokemonList =
    pokemonList |> findWithMethod searchTerm String.startsWith


fuzzyFindByName : String -> List Pokemon -> List Pokemon
fuzzyFindByName searchTerm pokemonList =
    pokemonList
        |> findWithMethod searchTerm String.contains
        |> List.append (pokemonList |> findByName searchTerm)
        |> LX.uniqueBy .name


pokemonMatchesTypes : ( Maybe String, Maybe String ) -> Pokemon -> Bool
pokemonMatchesTypes ( type1, type2 ) pokemon =
    let
        matchResult =
            [ type1, type2 ]
                |> List.filterMap identity
                |> List.map (matchesPokemonType pokemon)
    in
    ((matchResult |> List.length) > 0)
        && (matchResult |> List.all (\a -> a == True))


matchesPokemonType : Pokemon -> String -> Bool
matchesPokemonType pokemon typeName =
    case pokemon.pokeType of
        Single t1 ->
            t1.name == typeName

        Dual t1 t2 ->
            [ t1, t2 ] |> List.map .name |> List.member typeName


isStrongAgainst : Type -> List Type -> Pokemon -> Bool
isStrongAgainst checkType typeList pokemon =
    typeList
        |> List.filter
            (\t ->
                case pokemon.pokeType of
                    Single t1 ->
                        t.name == t1.name

                    Dual t1 t2 ->
                        t.name == t1.name || t.name == t2.name
            )
        |> List.any (\t -> t.strengths |> List.member checkType.name)



-- PokemonType


type PokemonType
    = Single Type
    | Dual Type Type


comparePokemonTypeStrengths : ( PokemonType, List String ) -> ( PokemonType, List String ) -> Order
comparePokemonTypeStrengths ( type1, strengths1 ) ( type2, strengths2 ) =
    if compare (List.length strengths1) (List.length strengths2) == EQ then
        comparePokemonTypes type1 type2

    else
        compare (List.length strengths1) (List.length strengths2)


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
        |> LX.uniqueBy (\pokeType -> pokeType |> typeAsList |> List.map .name |> List.sort)
        |> List.map
            (\pokeType -> ( pokeType, pokeType |> buildTypeStrengths allTypes ))


buildTypeStrengths : List Type -> PokemonType -> List String
buildTypeStrengths allTypes pokeType =
    pokeType
        |> typeAsList
        |> List.map .strengths
        |> List.concat
        |> LX.unique


buildWeaknessList : PokemonType -> List String
buildWeaknessList pokemonType =
    pokemonType
        |> typeAsList
        |> List.map .weaknesses
        |> List.concat
        |> LX.unique


buildStrengthList : PokemonType -> List String
buildStrengthList pokemonType =
    pokemonType
        |> typeAsList
        |> List.map .strengths
        |> List.concat
        |> LX.unique



-- Type


type alias Type =
    { name : String
    , strengths : List String
    , weaknesses : List String
    , ineffectives : List String
    , noEffects : List String
    }


namesToTypes : Dict String Type -> List String -> List Type
namesToTypes typeMap typeNames =
    typeNames
        |> List.map (\name -> Dict.get name typeMap)
        |> List.filterMap identity


typeAsList : PokemonType -> List Type
typeAsList pokemonType =
    case pokemonType of
        Single type1 ->
            type1 |> List.singleton

        Dual type1 type2 ->
            [ type1, type2 ]



-- Offenses


type alias Offenses =
    { x2 : List String
    , half : List String
    }


initOffenses : Offenses
initOffenses =
    { x2 = []
    , half = []
    }



-- Defenses


type alias Defenses =
    { x4 : List String
    , x2 : List String
    , x0 : List String
    , half : List String
    , quarter : List String
    }


initDefenses : Defenses
initDefenses =
    { x4 = []
    , x2 = []
    , x0 = []
    , half = []
    , quarter = []
    }
