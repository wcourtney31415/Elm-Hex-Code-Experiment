module Main exposing (..)

{-| -}

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import Element.Lazy


hexCharToInt : Char -> Maybe Int
hexCharToInt char =
    let
        validChars =
            "0123456789abcdef"

        indexedList =
            List.indexedMap Tuple.pair <| String.toList validChars

        searchResult =
            List.head <|
                List.filter
                    (\( _, value ) -> value == char)
                    indexedList
    in
    case searchResult of
        Nothing ->
            Nothing

        Just ( result, _ ) ->
            Just result


splitPair : String -> Maybe ( Char, Char )
splitPair str =
    let
        list =
            String.toList <| String.toLower str

        head =
            List.head list

        last =
            List.head <| List.reverse list
    in
    case head of
        Nothing ->
            Nothing

        Just first ->
            case last of
                Nothing ->
                    Nothing

                Just second ->
                    Just ( first, second )


charTuptoIntTup : Maybe ( Char, Char ) -> Maybe ( Int, Int )
charTuptoIntTup charTup =
    case charTup of
        Nothing ->
            Nothing

        Just tup ->
            let
                possibleA =
                    hexCharToInt <| Tuple.first tup

                possibleB =
                    hexCharToInt <| Tuple.second tup
            in
            case possibleA of
                Nothing ->
                    Nothing

                Just a ->
                    case possibleB of
                        Nothing ->
                            Nothing

                        Just b ->
                            Just ( a, b )


pairRebase : Maybe ( Int, Int ) -> Maybe Int
pairRebase tuple =
    let
        ret =
            case tuple of
                Nothing ->
                    Nothing

                Just ( charA, charB ) ->
                    Just (charA * 16 + charB)
    in
    ret


hex : String -> Color
hex hexCode =
    let
        firstChar =
            List.head <| String.toList hexCode

        rPair =
            String.slice 1 3 hexCode

        gPair =
            String.slice 3 5 hexCode

        bPair =
            String.slice 5 7 hexCode

        rSplit =
            splitPair rPair

        gSplit =
            splitPair gPair

        bSplit =
            splitPair bPair

        rNumber =
            pairRebase <| charTuptoIntTup rSplit

        gNumber =
            pairRebase <| charTuptoIntTup gSplit

        bNumber =
            pairRebase <| charTuptoIntTup bSplit

        red =
            rgb255 255 0 0

        ret =
            if String.length hexCode == 7 then
                case firstChar of
                    Just '#' ->
                        case rNumber of
                            Nothing ->
                                red

                            Just r ->
                                case gNumber of
                                    Nothing ->
                                        red

                                    Just g ->
                                        case bNumber of
                                            Nothing ->
                                                red

                                            Just b ->
                                                rgb255 r g b

                    _ ->
                        red

            else
                red
    in
    ret


main =
    let
        hexString =
            "#fa6600"

        myColor =
            hex hexString
    in
    Element.layout
        [ Background.color myColor
        , Font.size 60
        ]
    <|
        el
            [ centerX, centerY ]
            (text hexString)
