module Main exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input
import Element.Lazy
import Hex exposing (..)


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
