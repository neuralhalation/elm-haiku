module Main exposing (basicCount, checkForY, matchSyllables, userReplace)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, string, list)
import List.Extra as List
import Regex
import Random.Extra exposing (filter)
import Random exposing (Generator)


{- Syllable counting -}


userReplace : String -> (Regex.Match -> String) -> String -> String
userReplace userRegex replacer string =
    case Regex.fromString userRegex of
        Nothing ->
            string

        Just regex ->
            Regex.replace regex replacer string


basicCount : String -> String
basicCount word =
    userReplace "(?:[^laeiouy]es|ed|[^laeiouy]e)$" (\_ -> "") word


checkForY word =
    userReplace "^y" (\_ -> "") word


matchSyllables =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[aeiouy]{1,2}"

match : String -> List Regex.Match
match word =
    Regex.find matchSyllables (checkForY (basicCount word))

matchList : List String -> List (List Regex.Match)
matchList words =
    List.map match words

getLengths : List (List a) -> List Int
getLengths ml =
    List.map List.length ml

lengths : List String -> List Int
lengths words =
    getLengths (matchList words)

pair : List String -> List (String, Int)
pair words =
    List.map2 Tuple.pair words (lengths words)

numOfSylEq : a -> (a1, a) -> Bool
numOfSylEq length wordTuple =
    Tuple.second wordTuple == length

userShuffle : List String -> List String
userShuffle words =
    List.sortBy String.length words

hasSyllables syllables pairs =
    List.filter (numOfSylEq syllables) pairs


syllableWord : Int -> List String -> Maybe ( String, Int )
syllableWord syllables words =
    let
        ws = userShuffle words
    in
        List.head (hasSyllables syllables (pair ws))

nextSyllableWord : Int -> List String -> Maybe ( String, Int )
nextSyllableWord syllables words =
    let
        ws = List.tail (List.take 5 (pair words))
    in
        case ws of
            Nothing ->
                Maybe.Just ("fuck", 1)
            Just w ->
                List.head (hasSyllables syllables w)

thirdSyllableWord : Int -> List String -> Maybe ( String, Int )
thirdSyllableWord syllables words =
    let
        ws = List.tail (List.take 205 (pair words))
    in
        case ws of
            Nothing ->
                Maybe.Just ("fuck", 1)
            Just w ->
                List.head (hasSyllables syllables w)
    

firstLine words =
     [ syllableWord 2 words
     , syllableWord 3 words
     ]

secondLine words =
    [ nextSyllableWord 2 words
    , syllableWord 1 words
    , syllableWord 4 words
    ]

thirdLine words =
    [ thirdSyllableWord 4 words
    , nextSyllableWord 1 words
    ]

getWord wordTuple =
    case wordTuple of
        Nothing ->
            "fuck"
        Just tuple ->
            Tuple.first tuple

{- get words -}

getWordList : Cmd Msg
getWordList =
    Http.get
        { url = "https://api.noopschallenge.com/wordbot?count=1000"
        , expect = Http.expectJson GotWords wordDecoder
        }

wordDecoder : Decoder (List String)
wordDecoder =
    field "words" (list string)

type Msg
    = GetWords 
    | GotWords (Result Http.Error (List String))


type Model =
    Failure
    | Loading
    | Success (Maybe (List String))

init : () -> (Model, Cmd Msg)
init _ =
    (Loading, getWordList)


-- update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetWords  ->
            (Loading, getWordList)

        GotWords result ->
            case result of
                Ok words ->
                    (Success (Maybe.Just words), Cmd.none)
            
                Err _ ->
                    (Failure, Cmd.none)
                    

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- helpers

lineGenerator lines words =
    case words of
        Nothing ->
            "fuck"
        Just ws ->
            String.join " " ( List.map getWord (lines ws))

-- dom generation

firstLineHtml : Maybe (List String) -> Html Msg
firstLineHtml words =
    h4 [] [ text <| lineGenerator firstLine words ]

secondLineHtml : Maybe (List String) -> Html Msg
secondLineHtml words =
    h4 [] [ text <| lineGenerator secondLine words ]

thirdLineHtml : Maybe (List String) -> Html Msg
thirdLineHtml words =
    h4 [] [ text <| lineGenerator thirdLine words ]

view : Model -> Html Msg
view model =
    mainContent model

mainContent : Model -> Html Msg
mainContent model =
    case model of
        Failure ->
            div []
            [ text "I can't load a shitty three word haiku for some reason..."
            , Button.button [Button.onClick GetWords] [text "Try again!"]
            ]
        
        Loading ->
            text "Loading haiku..."

        Success words ->
            div []
                [ h1 [] [ text "shitty haikus" ]
                , firstLineHtml words
                , secondLineHtml words
                , thirdLineHtml words
                , Button.button [ Button.onClick GetWords ] [ text "inspire me more" ]
                ]       


-- main

main =
    Browser.element {init = init
                    , update = update
                    , subscriptions = subscriptions
                    , view = view
                    }
