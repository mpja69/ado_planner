module Grid.Update exposing (update)

import Grid.Logic as GL
import Grid.Msg as GM
import Grid.Types as GT
import Types exposing (AdoCmd, Iteration(..))


update : GM.Msg -> GT.Model -> ( GT.Model, List AdoCmd )
update msg model =
    case msg of
        GM.NoOp ->
            ( model, [] )

        GM.DeliveryDragStart fid ->
            ( { model | draggingDelivery = Just fid, hoverDeliverySprint = Nothing }, [] )

        GM.DeliveryDragEnd ->
            ( { model | draggingDelivery = Nothing, hoverDeliverySprint = Nothing }, [] )

        GM.HoverDelivery mIx ->
            ( { model | hoverDeliverySprint = mIx }, [] )

        GM.DeliveryDrop newIx ->
            case model.draggingDelivery of
                Nothing ->
                    ( model, [] )

                Just fid ->
                    let
                        up r =
                            if r.featureId == fid then
                                { r | iteration = InPI newIx }

                            else
                                r

                        rows2 =
                            List.map up model.rows

                        intents =
                            [ Types.SetIteration { id = fid, toSprintIx = newIx } ]
                    in
                    ( { model
                        | rows = rows2
                        , draggingDelivery = Nothing
                        , hoverDeliverySprint = Nothing
                      }
                    , intents
                    )

        GM.StoryDragStart sid ->
            ( { model | draggingStory = Just sid, hoverStorySprint = Nothing }, [] )

        GM.StoryDragEnd ->
            ( { model | draggingStory = Nothing, hoverStorySprint = Nothing }, [] )

        GM.HoverStory mIx ->
            ( { model | hoverStorySprint = mIx }, [] )

        GM.StoryDrop newIx ->
            case model.draggingStory of
                Nothing ->
                    ( model, [] )

                Just sid ->
                    let
                        upS s =
                            if s.id == sid then
                                { s | iteration = InPI newIx }

                            else
                                s

                        upR r =
                            { r | stories = List.map upS r.stories }

                        rows2 =
                            List.map upR model.rows

                        intents =
                            [ Types.SetIteration { id = sid, toSprintIx = newIx } ]
                    in
                    ( { model
                        | rows = rows2
                        , draggingStory = Nothing
                        , hoverStorySprint = Nothing
                      }
                    , intents
                    )

        GM.ToggleTest fid kind ->
            let
                rows2 =
                    List.map (GL.toggleTest fid kind) model.rows

                updated =
                    List.filter (\r -> r.featureId == fid) rows2 |> List.head

                intents =
                    case updated of
                        Just r2 ->
                            [ Types.SetFeatureTags
                                { featureId = fid
                                , sit = r2.tests.sit
                                , uat = r2.tests.uat
                                , e2e = r2.tests.e2e
                                }
                            ]

                        Nothing ->
                            []
            in
            ( { model | rows = rows2 }, intents )
