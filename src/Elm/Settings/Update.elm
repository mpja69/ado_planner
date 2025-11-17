module Settings.Update exposing (update)

import Config
import Settings.Msg as SM
import Settings.Types as ST


update : SM.Msg -> ST.Model -> ( ST.Model, Config.Config -> Config.Config )
update msg model =
    case msg of
        SM.ToggleOpen ->
            ( { model | isOpen = not model.isOpen }
            , identity
            )

        SM.SetEnableTests flag ->
            ( { model | enableTests = flag }
            , \cfg -> ST.toConfig { model | enableTests = flag } cfg
            )

        SM.SetSitTag s ->
            ( { model | sitTag = s }
            , \cfg -> ST.toConfig { model | sitTag = s } cfg
            )

        SM.SetUatTag s ->
            ( { model | uatTag = s }
            , \cfg -> ST.toConfig { model | uatTag = s } cfg
            )

        SM.SetE2eTag s ->
            ( { model | e2eTag = s }
            , \cfg -> ST.toConfig { model | e2eTag = s } cfg
            )

        SM.SetEnableTeamTags flag ->
            ( { model | enableTeamTags = flag }
            , \cfg -> ST.toConfig { model | enableTeamTags = flag } cfg
            )

        SM.SetTeamTagsInput s ->
            ( { model | teamTagsInput = s }
            , \cfg -> ST.toConfig { model | teamTagsInput = s } cfg
            )
