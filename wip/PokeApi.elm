module PokeApi exposing (Pokemon)

import Dict exposing (Dict)
import Http
import Json.Decode as JD exposing (..)



{- Types -}


type alias NamedApiResource =
    { name : String
    , url : String
    }


type alias Pokemon =
    { id_ : Int
    , name : String
    , abilities : List Ability
    , sprites : PokemonSprite
    , species : NamedApiResource
    , types : Type
    }


type alias Ability =
    { isHidden : Bool
    , slot : Int
    , ability : NamedApiResource
    }


type alias PokemonSprite =
    { frontDefault : String }


type alias PokemonSpecies =
    { id_ : Int
    , name : String
    , captureRate : Int
    , isBaby : Bool
    , growthRate : NamedApiResource
    , pokedexNumbers : List DexEntry
    , eggGroups : List NamedApiResource
    , evolvesFromSpecies : NamedApiResource
    , generation : NamedApiResource
    }


type alias DexEntry =
    { entryNumber : Int
    , pokedex : NamedApiResource
    }


type alias Pokedex =
    { id_ : Int
    , name : String
    , pokemonEntries : List PokemonEntry
    }


type alias PokemonEntry =
    { entryNumber : Int
    , pokemonSpecies : NamedApiResource
    }


type alias Type =
    { id_ : Int
    , name : String
    , damageRelations : TypeRelations
    , pokemon : List TypePokemon
    }


type alias TypeRelations =
    { noDamageTo : NamedApiResource
    , halfDamageTo : NamedApiResource
    , doubleDamageTo : NamedApiResource
    , noDamageFrom : NamedApiResource
    , halfDamageFrom : NamedApiResource
    , doubleDamageFrom : NamedApiResource
    }


type alias TypePokemon =
    { pokemon : NamedApiResource }



{- Decoders -}


decodeNamedApiResource : JD.Decoder NamedApiResource
decodeNamedApiResource =
    JD.map2 NamedApiResource
        (JD.field "name" JD.string)
        (JD.field "url" JD.string)


decodePokemon : JD.Decoder Pokemon
decodePokemon =
    JD.map6 Pokemon
        (JD.field "id_" JD.int)
        (JD.field "name" JD.string)
        (JD.field "abilities" (JD.list decodeAbility))
        (JD.field "sprites" decodePokemonSprite)
        (JD.field "species" decodePokemonSpecies)
        (JD.field "types" decodeType)


decodeAbility : JD.Decoder Ability
decodeAbility =
    JD.map3 Ability
        (JD.field "isHidden" JD.bool)
        (JD.field "slot" JD.int)
        (JD.field "ability" decodeNamedApiResource)


decodePokemonSprite : JD.Decoder PokemonSprite
decodePokemonSprite =
    JD.map PokemonSprite
        (JD.field "frontDefault" JD.string)


decodePokemonSpecies : JD.Decoder PokemonSpecies
decodePokemonSpecies =
    JD.map9 PokemonSpecies
        (JD.field "id_" JD.int)
        (JD.field "name" JD.string)
        (JD.field "captureRate" JD.int)
        (JD.field "isBaby" JD.bool)
        (JD.field "growthRate" decodeNamedApiResource)
        (JD.field "pokedexNumbers" (JD.list decodeDexEntry))
        (JD.field "eggGroups" (JD.list decodeNamedApiResource))
        (JD.field "evolvesFromSpecies" decodeNamedApiResource)
        (JD.field "generation" decodeNamedApiResource)


decodeDexEntry : JD.Decoder DexEntry
decodeDexEntry =
    JD.map2 DexEntry
        (JD.field "entryNumber" JD.int)
        (JD.field "pokedex" decodePokedex)


decodePokedex : JD.Decoder Pokedex
decodePokedex =
    JD.map3 Pokedex
        (JD.field "id_" JD.int)
        (JD.field "name" JD.string)
        (JD.field "pokemonEntries" (JD.list decodePokemonEntry))


decodePokemonEntry : JD.Decoder PokemonEntry
decodePokemonEntry =
    JD.map2 PokemonEntry
        (JD.field "entryNumber" JD.int)
        (JD.field "pokemonSpecies" decodeNamedApiResource)


decodeType : JD.Decoder Type
decodeType =
    JD.map4 Type
        (JD.field "id_" JD.int)
        (JD.field "name" JD.string)
        (JD.field "damageRelations" decodeTypeRelations)
        (JD.field "pokemon" (JD.list decodeTypePokemon))


decodeTypeRelations : JD.Decoder TypeRelations
decodeTypeRelations =
    JD.map6 TypeRelations
        (JD.field "noDamageTo" decodeNamedApiResource)
        (JD.field "halfDamageTo" decodeNamedApiResource)
        (JD.field "doubleDamageTo" decodeNamedApiResource)
        (JD.field "noDamageFrom" decodeNamedApiResource)
        (JD.field "halfDamageFrom" decodeNamedApiResource)
        (JD.field "doubleDamageFrom" decodeNamedApiResource)


decodeTypePokemon : JD.Decoder TypePokemon
decodeTypePokemon =
    JD.map TypePokemon
        (JD.field "pokemon" decodeNamedApiResource)



{- URL Endpoint Creation -}


urlBase : String
urlBase =
    "https://pokeapi.co/api/v2"


paginate : Int -> String -> String
paginate page url =
    url
        ++ "?offset="
        ++ String.fromInt (page * 20)


paginateWithCount : Int -> Int -> String -> String
paginateWithCount page itemCount url =
    url
        ++ "?limit="
        ++ String.fromInt itemCount
        ++ "&offset="
        ++ String.fromInt (page * itemCount)


buildMethodUrl : String -> Method -> String
buildMethodUrl suffix method =
    (case method of
        GetAll ->
            [ urlBase, suffix ]

        GetById id_ ->
            [ urlBase, suffix, String.fromInt id_ ]

        GetByName name ->
            [ urlBase, suffix, name ]
    )
        |> String.join "/"


buildUrl : Endpoint -> String
buildUrl endpoint =
    case endpoint of
        PokemonEndpoint method ->
            method |> buildMethodUrl "pokemon"

        PokemonSpeciesEndpoint method ->
            method |> buildMethodUrl "pokemon-species"

        TypeEndpoint method ->
            method |> buildMethodUrl "type"


type Method
    = GetAll
    | GetById Int
    | GetByName String


type Endpoint
    = PokemonEndpoint Method
    | PokemonSpeciesEndpoint Method
    | TypeEndpoint Method
