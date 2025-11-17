module Filters.AreaSelector exposing
    ( AreaMini
    , Model
    , Msg(..)
    , displayName
    , init
    , update
    , view
    )

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Ui.Icons as I



-- TYPES


type alias AreaMini =
    { id : String -- full path, e.g. "Contoso\\ART"
    , name : String -- short label, e.g. "ART"
    }


type alias Model =
    { selectedId : Maybe String
    , areas : List AreaMini
    , favorites : Set String -- favorite *ids*
    , query : String
    , dropdownOpen : Bool
    }


type Msg
    = ToggleDropdown
    | UpdateQuery String
    | SelectArea String
    | ReplaceAreas (List AreaMini)
    | ReplaceFavorites (List String)



-- INIT


init : { selectedId : Maybe String, areas : List AreaMini, favorites : List String } -> Model
init cfg =
    { selectedId = cfg.selectedId
    , areas = cfg.areas
    , favorites = Set.fromList cfg.favorites
    , query = ""
    , dropdownOpen = False
    }



-- UPDATE


update : Msg -> Model -> ( Model, Maybe String )
update msg m =
    case msg of
        ReplaceAreas xs ->
            ( { m | areas = xs }, Nothing )

        ReplaceFavorites favNames ->
            ( { m | favorites = Set.fromList (List.map String.toLower favNames) }, Nothing )

        ToggleDropdown ->
            ( { m | dropdownOpen = not m.dropdownOpen, query = "" }, Nothing )

        UpdateQuery q ->
            ( { m | query = q }, Nothing )

        SelectArea areaId ->
            ( { m | selectedId = Just areaId, dropdownOpen = False }, Just areaId )



-- VIEW


view : Model -> Html Msg
view model =
    let
        selectedLabel =
            case model.selectedId of
                Just id_ ->
                    case List.filter (\t -> t.id == id_) model.areas |> List.head of
                        Just t ->
                            displayName t

                        Nothing ->
                            "Choose ART"

                Nothing ->
                    "Choose ART"

        dropdownContent =
            if model.dropdownOpen then
                [ Html.div
                    [ A.class "absolute top-full left-0 mt-1 w-[380px] bg-white shadow-lg rounded border border-gray-200 max-h-80 overflow-y-auto z-50" ]
                    (renderDropdown model)
                ]

            else
                []
    in
    Html.div [ A.class "w-full flex justify-start" ]
        [ Html.div [ A.class "relative z-30" ]
            (viewToggleButton selectedLabel model.dropdownOpen :: dropdownContent)
        ]


renderDropdown : Model -> List (Html Msg)
renderDropdown model =
    let
        q =
            String.toLower model.query

        matches t =
            let
                name =
                    String.toLower (displayName t)
            in
            String.contains q name

        visible =
            List.filter matches model.areas

        ( favs, others ) =
            List.partition (isFavorite model.favorites) visible
    in
    [ renderSearchBar model.query ]
        ++ renderSection "My favorite ARTs" model.favorites model.selectedId favs
        ++ renderSection "All ARTs" model.favorites model.selectedId others


renderSearchBar : String -> Html Msg
renderSearchBar query =
    Html.div
        [ A.class "sticky top-0 bg-white z-10 border-b border-gray-200 p-3" ]
        [ Html.input
            [ A.placeholder "Search for ART"
            , A.value query
            , E.onInput UpdateQuery
            , A.class "w-full px-2 py-1 border border-gray-300 rounded"
            ]
            []
        ]


renderSection : String -> Set String -> Maybe String -> List AreaMini -> List (Html Msg)
renderSection title favIds selectedId items =
    if List.isEmpty items then
        []

    else
        Html.div [ A.class "font-semibold text-gray-700 px-3 pt-3 pb-1" ] [ Html.text title ]
            :: List.map (renderItem favIds selectedId) items


renderItem : Set String -> Maybe String -> AreaMini -> Html Msg
renderItem favIds selectedId area =
    let
        selected =
            isSelected selectedId area

        star =
            if isFavorite favIds area then
                Html.span [ A.class "text-yellow-500 mr-2" ] [ Html.text "★" ]

            else
                Html.text ""
    in
    Html.div
        [ E.onClick (SelectArea area.id)
        , A.class "px-4 py-2 cursor-pointer hover:bg-gray-100 text-sm"
        , A.attribute "style"
            ("background-color:"
                ++ (if selected then
                        "#e6f2ff"

                    else
                        "white"
                   )
            )
        ]
        [ Html.div [ A.class "flex items-center justify-between" ]
            [ Html.span [ A.class "w-5 flex justify-left" ]
                [ if selected then
                    I.checkmark [ Attr.class "w-4 h-4" ]

                  else
                    Html.text ""
                ]
            , Html.span [ A.class "flex-1 ml-2" ] [ Html.text (displayName area) ]
            , star
            ]
        ]


viewToggleButton : String -> Bool -> Html Msg
viewToggleButton selectedLabel isOpen =
    Html.button
        [ A.class
            "w-[260px] inline-flex items-center gap-2 px-3 py-2 rounded border border-gray-300 bg-white hover:bg-gray-50"
        , E.onClick ToggleDropdown
        , A.attribute "aria-expanded"
            (if isOpen then
                "true"

             else
                "false"
            )
        ]
        [ I.board []
        , Html.span
            [ A.class "w-[200px] overflow-hidden text-ellipsis whitespace-nowrap text-left font-semibold text-xs" ]
            [ Html.text selectedLabel ]
        , I.chevronDown []
        ]



-- HELPERS


displayName : AreaMini -> String
displayName t =
    t.name


isSelected : Maybe String -> AreaMini -> Bool
isSelected sel t =
    case sel of
        Just id_ ->
            id_ == t.id

        Nothing ->
            False


isFavorite : Set String -> AreaMini -> Bool
isFavorite favIds area =
    Set.member area.id favIds
