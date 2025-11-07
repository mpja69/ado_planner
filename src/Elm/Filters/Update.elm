module Filters.Update exposing (update)

import Data.Ado as Ado
import Data.Filter as F
import Filters.AreaSelector as AS
import Filters.Logic as FL
import Filters.Msg as FM
import Filters.Types as FT exposing (allTagsL, areaL, includeTagsL, iterationL, iterationsL, optionsL, selectionL, tagQueryL, uiStateL)
import Lens exposing (compose, set)
import Set


update : FM.Msg -> FT.Model -> FT.Model
update msg model =
    case msg of
        FM.ToggleOpen ->
            { model | isOpen = not model.isOpen }

        FM.AreaSel asMsg ->
            let
                ( areaSel2, chosen ) =
                    AS.update asMsg model.areaSel

                model1 =
                    { model | areaSel = areaSel2 }
            in
            case chosen of
                Nothing ->
                    model1

                Just fullArea ->
                    model1
                        |> set (compose selectionL areaL) (Just fullArea)
                        -- rensa taggar här om du vill att listan ska bli grå tills fetch är klar
                        |> set (compose selectionL includeTagsL) Set.empty
                        |> set allTagsL []

        FM.SetIteration piRoot ->
            model
                |> set (compose selectionL iterationL) (Just piRoot)        
        -- FM.SetIteration piRoot ->
        --     let
        --         model1 =
        --             model
        --                 |> set (compose selectionL iterationL) (Just piRoot)
        --
        --         newTags =
        --             FL.deriveTagsFromADO
        --                 { area = model1.sel.area, iteration = model1.sel.iteration }
        --                 Ado.sample
        --
        --         keepSet =
        --             Set.intersect model1.sel.includeTags (Set.fromList newTags)
        --     in
        --     model1
        --         |> set (compose selectionL includeTagsL) keepSet
        --         |> set allTagsL newTags

        -- Toggle i includeTags
        FM.ToggleTag tag ->
            let
                newSet =
                    if Set.member tag model.sel.includeTags then
                        Set.remove tag model.sel.includeTags

                    else
                        Set.insert tag model.sel.includeTags
            in
            model
                |> set (compose selectionL includeTagsL) newSet

        -- Söksträng för taggar
        FM.SetTagQuery q ->
            model
                |> set (compose uiStateL tagQueryL) q

        FM.ClearTagQuery ->
            model
                |> set (compose uiStateL tagQueryL) ""

        FM.NoOp ->
            model
