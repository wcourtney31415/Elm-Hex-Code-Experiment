module Main exposing (..)

{-| -}

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import Element.Lazy


hex : String -> Color
hex hexCode =
    let
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

                Just ( integer, _ ) ->
                    Just integer

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

        charTupToIntTup : Maybe ( Char, Char ) -> Maybe ( Int, Int )
        charTupToIntTup charTup =
            case charTup of
                Nothing ->
                    Nothing

                Just ( tupA, tupB ) ->
                    let
                        mbyA =
                            hexCharToInt tupA

                        mbyB =
                            hexCharToInt tupB
                    in
                    case mbyA of
                        Nothing ->
                            Nothing

                        Just a ->
                            case mbyB of
                                Nothing ->
                                    Nothing

                                Just b ->
                                    Just ( a, b )

        toBase10 : Maybe ( Int, Int ) -> Maybe Int
        toBase10 tuple =
            case tuple of
                Nothing ->
                    Nothing

                Just ( a, b ) ->
                    Just (a * 16 + b)

        firstChar =
            List.head <| String.toList hexCode

        maybeR =
            toBase10 <|
                charTupToIntTup <|
                    splitPair <|
                        String.slice 1 3 hexCode

        maybeG =
            toBase10 <|
                charTupToIntTup <|
                    splitPair <|
                        String.slice 3 5 hexCode

        maybeB =
            toBase10 <|
                charTupToIntTup <|
                    splitPair <|
                        String.slice 5 7 hexCode

        failColor =
            rgb255 255 0 0
    in
    if String.length hexCode == 7 then
        case firstChar of
            Just '#' ->
                case maybeR of
                    Nothing ->
                        failColor

                    Just r ->
                        case maybeG of
                            Nothing ->
                                failColor

                            Just g ->
                                case maybeB of
                                    Nothing ->
                                        failColor

                                    Just b ->
                                        rgb255 r g b

            _ ->
                failColor

    else
        failColor


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
