module Ui exposing
    ( UiColor(..)
    , UiSize(..)
    , cardToneForStatus
    , statusPill
    , statusToneClasses
    , storyIterationBadge
    , testChipView
    , testStripView
    , uiToneClasses
    , warnBadge
    , warnToneClasses
    )

import Html exposing (Html, div, span, text)
import Html.Attributes as A
import Types exposing (FeatureWarn(..), Iteration(..), Status(..), Tests)



-- COLORS (semantic)


type UiColor
    = Slate
    | Sky
    | Emerald
    | Amber
    | Rose
    | Indigo



-- SIZES (only what we need)


type UiSize
    = Tiny
    | Small
    | Medium



-- Tailwind classes per semantic color (border + bg + text)


uiToneClasses : UiColor -> String
uiToneClasses color =
    case color of
        Slate ->
            "border-slate-200 bg-slate-100 text-slate-700"

        Sky ->
            "border-sky-200 bg-sky-100 text-sky-900"

        Emerald ->
            "border-emerald-200 bg-emerald-100 text-emerald-900"

        Amber ->
            "border-amber-300 bg-amber-50 text-amber-800"

        Rose ->
            "border-rose-300 bg-rose-50 text-rose-800"

        Indigo ->
            "border-indigo-300 bg-indigo-100 text-indigo-900"



-- Map domain -> UiColor


statusToneClasses : Status -> String
statusToneClasses st =
    case st of
        Todo ->
            uiToneClasses Slate

        Doing ->
            uiToneClasses Sky

        Done ->
            uiToneClasses Emerald


warnToneClasses : FeatureWarn -> String
warnToneClasses w =
    case w of
        WarnNeedsDelivery ->
            uiToneClasses Amber

        WarnAfter ->
            uiToneClasses Amber

        WarnStoriesNotDone ->
            uiToneClasses Rose

        WarnFeatureLagging ->
            uiToneClasses Amber

        NoWarn ->
            uiToneClasses Slate



-- Card tint helper (use on story/feature cards)
-- Example: A.class ("rounded-xl border " ++ cardToneForStatus row.status)


cardToneForStatus : Status -> String
cardToneForStatus st =
    statusToneClasses st



-- Size presets for pills/badges


badgeSizeClasses : UiSize -> String
badgeSizeClasses sz =
    case sz of
        Tiny ->
            "px-1 py-0 text-[10px]"

        Small ->
            "px-1.5 py-0.5 text-[11px]"

        Medium ->
            "px-2 py-[3px] text-[12px]"



-- Tätare storlekar för CHIPS (små, kompakta klick-etiketter)


chipSizeClasses : UiSize -> String
chipSizeClasses sz =
    case sz of
        Tiny ->
            "px-[3px] py-0 text-[9px]"

        Small ->
            "px-[6px] py-[1px] text-[10px]"

        Medium ->
            "px-[8px] py-[2px] text-[11px]"



-- STATUS PILL (non-interactive)


statusPill : UiSize -> Status -> Html msg
statusPill sz st =
    let
        tone =
            statusToneClasses st

        label =
            case st of
                Todo ->
                    "Todo"

                Doing ->
                    "Doing"

                Done ->
                    "Done"
    in
    span
        [ A.class ("inline-flex items-center rounded-md border " ++ badgeSizeClasses sz ++ " " ++ tone)
        , A.title ("Feature status: " ++ label)
        ]
        [ text label ]



-- WARNING BADGE (non-Maybe version; NoWarn renders an empty span)


warnBadge : UiSize -> FeatureWarn -> Html msg
warnBadge sz kind =
    case kind of
        NoWarn ->
            span [] []

        _ ->
            let
                tone =
                    warnToneClasses kind

                label =
                    case kind of
                        WarnNeedsDelivery ->
                            "⚠ set delivery"

                        WarnAfter ->
                            "⚠ after"

                        WarnStoriesNotDone ->
                            "⚠ stories"

                        WarnFeatureLagging ->
                            "⚠ start feature"

                        NoWarn ->
                            ""

                -- unreachable due to earlier case
            in
            span
                [ A.class ("inline-flex items-center rounded-md border " ++ badgeSizeClasses sz ++ " " ++ tone)
                , A.title label
                ]
                [ text label ]



--  test-chip and test-strip-------------------------------------------


testChipView : UiSize -> { label : String, active : Bool } -> List (Html.Attribute msg) -> Html msg
testChipView sz cfg attrs =
    let
        base =
            "inline-flex items-center justify-center rounded-full border cursor-pointer select-none transition "
                ++ chipSizeClasses sz

        tone =
            if cfg.active then
                uiToneClasses Indigo

            else
                uiToneClasses Slate
    in
    span
        (A.class (base ++ " " ++ tone) :: attrs)
        [ text cfg.label ]


testStripView : UiSize -> List (Html msg) -> Html msg
testStripView size chips =
    case size of
        Tiny ->
            div [ A.class "inline-flex items-center gap-0.5" ] chips

        Small ->
            div [ A.class "inline-flex items-center gap-1" ] chips

        Medium ->
            div [ A.class "inline-flex items-center gap-1.5" ] chips



-- Iteration -------------------------------------------


iterationToneColor : Iteration -> Maybe UiColor
iterationToneColor iter =
    case iter of
        InPI _ ->
            Nothing

        Missing ->
            Just Amber

        WholePI ->
            Just Amber

        OutsidePI ->
            Just Rose



-- Iteration → (valfri) label


iterationLabel : Iteration -> Maybe String
iterationLabel iter =
    case iter of
        InPI _ ->
            Nothing

        Missing ->
            Just "⚠ no sprint"

        WholePI ->
            Just "⚠ whole PI"

        OutsidePI ->
            Just "⚠ wrong PI"



-- Badge för storyns iteration-varning (NoWarn-fall ger tom span)


storyIterationBadge : UiSize -> Iteration -> Html msg
storyIterationBadge sz iter =
    case ( iterationToneColor iter, iterationLabel iter ) of
        ( Nothing, _ ) ->
            span [] []

        -- inget att visa
        ( Just toneColor, Just label ) ->
            span
                [ A.class
                    ("inline-flex items-center rounded-md border "
                        ++ badgeSizeClasses sz
                        ++ " "
                        ++ uiToneClasses toneColor
                    )
                , A.title label
                ]
                [ text label ]

        -- teoretiskt om färg men ingen label
        ( Just _, Nothing ) ->
            span [] []
