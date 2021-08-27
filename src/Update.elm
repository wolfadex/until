module Update exposing
    ( UpdateTuple
    , addMsg
    , fromModel
    , map
    , save
    , updateHelper
    )


type UpdateTuple model msg
    = UpdateTuple model (List (Cmd msg))


updateHelper : (msg -> UpdateTuple modelA msg -> UpdateTuple modelB msg) -> msg -> modelA -> UpdateTuple modelB msg
updateHelper fn msg model =
    fn msg (UpdateTuple model [])


fromModel : model -> UpdateTuple model msg
fromModel model =
    UpdateTuple model []


save : UpdateTuple model msg -> ( model, Cmd msg )
save (UpdateTuple model msgs) =
    ( model, Cmd.batch msgs )


map : (modelA -> modelB) -> UpdateTuple modelA msg -> UpdateTuple modelB msg
map fn (UpdateTuple model msgs) =
    UpdateTuple (fn model) msgs


addMsg : (model -> Cmd msg) -> UpdateTuple model msg -> UpdateTuple model msg
addMsg fn (UpdateTuple model msgs) =
    UpdateTuple model (fn model :: msgs)
