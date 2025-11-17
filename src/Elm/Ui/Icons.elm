module Ui.Icons exposing
    ( board
    , checkmark
    , chevronDown
    , chevronRight
    )

import Svg exposing (Svg)
import Svg.Attributes as Attr


chevronDown : List (Svg.Attribute msg) -> Svg msg
chevronDown attrs =
    Svg.svg
        ([ Attr.viewBox "0 0 16 16"
         , Attr.fill "none"
         , Attr.stroke "currentColor"
         , Attr.strokeWidth "1.5"
         , Attr.class "w-4 h-4"
         ]
            ++ attrs
        )
        [ Svg.polyline
            [ Attr.points "4,6 8,10 12,6"
            , Attr.strokeLinecap "round"
            , Attr.strokeLinejoin "round"
            ]
            []
        ]


chevronRight : List (Svg.Attribute msg) -> Svg msg
chevronRight attrs =
    Svg.svg
        ([ Attr.viewBox "0 0 16 16"
         , Attr.fill "none"
         , Attr.stroke "currentColor"
         , Attr.strokeWidth "1.5"
         , Attr.class "w-4 h-4"
         ]
            ++ attrs
        )
        [ Svg.polyline
            [ Attr.points "6,4 10,8 6,12"
            , Attr.strokeLinecap "round"
            , Attr.strokeLinejoin "round"
            ]
            []
        ]


board : List (Svg.Attribute msg) -> Svg msg
board attrs =
    Svg.svg
        ([ Attr.viewBox "0 0 24 24"
         , Attr.fill "none"
         , Attr.stroke "currentColor"
         , Attr.strokeWidth "1"
         , Attr.class "w-4 h-4"
         ]
            ++ attrs
        )
        [ Svg.rect [ Attr.x "3", Attr.y "3", Attr.width "6", Attr.height "4", Attr.rx "1", Attr.fill "#666", Attr.stroke "#666" ] []
        , Svg.line [ Attr.x1 "4", Attr.y1 "5", Attr.x2 "8", Attr.y2 "5", Attr.stroke "#fff" ] []
        , Svg.rect [ Attr.x "3", Attr.y "9", Attr.width "6", Attr.height "4", Attr.rx "1", Attr.fill "#000" ] []
        , Svg.line [ Attr.x1 "4", Attr.y1 "11", Attr.x2 "8", Attr.y2 "11", Attr.stroke "#fff" ] []
        , Svg.rect [ Attr.x "3", Attr.y "15", Attr.width "6", Attr.height "4", Attr.rx "1", Attr.fill "#000" ] []
        , Svg.line [ Attr.x1 "4", Attr.y1 "17", Attr.x2 "8", Attr.y2 "17", Attr.stroke "#fff" ] []
        , Svg.rect [ Attr.x "11", Attr.y "3", Attr.width "6", Attr.height "4", Attr.rx "1", Attr.fill "#666", Attr.stroke "#666" ] []
        , Svg.line [ Attr.x1 "12", Attr.y1 "5", Attr.x2 "16", Attr.y2 "5", Attr.stroke "#fff" ] []
        , Svg.rect [ Attr.x "11", Attr.y "9", Attr.width "6", Attr.height "4", Attr.rx "1", Attr.fill "#666", Attr.stroke "#666" ] []
        , Svg.line [ Attr.x1 "12", Attr.y1 "11", Attr.x2 "16", Attr.y2 "11", Attr.stroke "#fff" ] []
        ]


checkmark : List (Svg.Attribute msg) -> Svg msg
checkmark attrs =
    Svg.svg
        ([ Attr.height "16"
         , Attr.fill "none"
         , Attr.stroke "gray"
         , Attr.strokeWidth "2"
         , Attr.strokeLinecap "round"
         , Attr.strokeLinejoin "round"
         , Attr.class "w-4 h-4"
         ]
            ++ attrs
        )
        [ Svg.path [ Attr.d "M3 8l3 3 7-7" ] [] ]
