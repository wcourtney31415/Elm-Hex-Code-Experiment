module Main exposing (..)

{-| -}

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import Element.Lazy


charToInt : Char -> Maybe Int
charToInt char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


splitPair : String -> Maybe ( Char, Char )
splitPair str =
    let
        list =
            String.toList <| String.toLower str

        a : Maybe Char
        a =
            List.head list

        b : Maybe Char
        b =
            List.head <| List.reverse list

        ret =
            case a of
                Nothing ->
                    Nothing

                Just first ->
                    case b of
                        Nothing ->
                            Nothing

                        Just second ->
                            Just ( first, second )
    in
    ret


charTuptoIntTup : Maybe ( Char, Char ) -> Maybe ( Int, Int )
charTuptoIntTup charTup =
    case charTup of
        Nothing ->
            Nothing

        Just tup ->
            let
                possibleA =
                    charToInt <| Tuple.first tup

                possibleB =
                    charToInt <| Tuple.second tup
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
