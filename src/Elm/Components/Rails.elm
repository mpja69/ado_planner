module Components.Rails exposing (..)

import Html exposing (Html, div)
import Html.Attributes as A
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
import Ui exposing (UiSize(..))



-- type RailSize
--     = Small
--     | Medium


type PatternMode
    = Regular
    | Staggered


type alias RailSpecPx =
    { widthPx : Int
    , heightPx : Int
    , padLeftPx : Int
    , padTopPx : Int
    , cols : Int
    , rows : Int
    , dotDiameterPx : Int
    , gapXPx : Int
    , gapYPx : Int
    , dotColor : String
    , bgColor : String
    , rounded : Bool
    , mode : PatternMode
    }


railSpec : UiSize -> PatternMode -> RailSpecPx
railSpec size mode =
    case size of
        Tiny ->
            { widthPx = 15
            , heightPx = 20
            , padLeftPx = 2
            , padTopPx = 7
            , cols = 2
            , rows = 2
            , dotDiameterPx = 2
            , gapXPx = 4
            , gapYPx = 2
            , dotColor = "rgba(99,102,241,0.50)"
            , bgColor = "transparent"
            , rounded = True
            , mode = mode
            }

        Small ->
            { widthPx = 15
            , heightPx = 20
            , padLeftPx = 2
            , padTopPx = 5
            , cols = 2
            , rows = 3
            , dotDiameterPx = 2
            , gapXPx = 4
            , gapYPx = 2
            , dotColor = "rgba(99,102,241,0.50)"
            , bgColor = "transparent"
            , rounded = True
            , mode = mode
            }

        Medium ->
            { widthPx = 15
            , heightPx = 20
            , padLeftPx = 2
            , padTopPx = 1
            , cols = 2
            , rows = 5
            , dotDiameterPx = 2
            , gapXPx = 4
            , gapYPx = 2
            , dotColor = "rgba(99,102,241,0.50)"
            , bgColor = "transparent"
            , rounded = True
            , mode = mode
            }



-- Absolute-pixel rail: 1 SVG unit == 1 CSS pixel. No scaling.


dotRailPx : RailSpecPx -> Html msg
dotRailPx p =
    let
        w =
            max 1 p.widthPx

        h =
            max 1 p.heightPx

        pl =
            max 0 p.padLeftPx

        pt =
            max 0 p.padTopPx

        cols =
            max 1 p.cols

        rows =
            max 1 p.rows

        d =
            max 1 p.dotDiameterPx

        r =
            d // 2

        gx =
            max 0 p.gapXPx

        gy =
            max 0 p.gapYPx

        cellW =
            d + gx

        cellH =
            d + gy

        startX =
            pl + r

        startY =
            pt + r

        range n =
            List.range 0 (n - 1)

        circlesRegular : List (Svg msg)
        circlesRegular =
            range cols
                |> List.concatMap
                    (\c ->
                        let
                            cx =
                                startX + c * cellW
                        in
                        range rows
                            |> List.map
                                (\rIdx ->
                                    let
                                        cy =
                                            startY + rIdx * cellH
                                    in
                                    Svg.circle
                                        [ SA.cx (String.fromInt cx)
                                        , SA.cy (String.fromInt cy)
                                        , SA.r (String.fromInt r)
                                        , SA.fill p.dotColor
                                        ]
                                        []
                                )
                    )

        circlesStaggered : List (Svg msg)
        circlesStaggered =
            range cols
                |> List.concatMap
                    (\c ->
                        let
                            cx =
                                startX + c * cellW

                            isOdd =
                                modBy 2 c == 1

                            rowsHere =
                                if isOdd then
                                    max 0 (rows - 1)

                                else
                                    rows

                            shiftY =
                                if isOdd then
                                    cellH // 2

                                else
                                    0
                        in
                        range rowsHere
                            |> List.map
                                (\rIdx ->
                                    let
                                        cy =
                                            startY + shiftY + rIdx * cellH
                                    in
                                    Svg.circle
                                        [ SA.cx (String.fromInt cx)
                                        , SA.cy (String.fromInt cy)
                                        , SA.r (String.fromInt r)
                                        , SA.fill p.dotColor
                                        ]
                                        []
                                )
                    )

        circles_ =
            case p.mode of
                Regular ->
                    circlesRegular

                Staggered ->
                    circlesStaggered

        roundedCls =
            if p.rounded then
                " rounded-l-xl"

            else
                ""
    in
    -- Outer container: fixed px, no scaling
    div
        [ A.class ("relative shrink-0" ++ roundedCls)
        , A.style "width" (String.fromInt w ++ "px")
        , A.style "height" (String.fromInt h ++ "px")
        , A.style "backgroundColor" p.bgColor
        , A.attribute "aria-hidden" "true"
        ]
        [ svg
            [ SA.viewBox ("0 0 " ++ String.fromInt w ++ " " ++ String.fromInt h)
            , SA.width (String.fromInt w)
            , SA.height (String.fromInt h)
            , SA.shapeRendering "geometricPrecision"
            ]
            circles_
        ]
