port module Ports exposing (storeModel, onStorageChange)

import Json.Encode exposing (Value)


port storeModel : Maybe String -> Cmd msg


port onStorageChange : (Value -> msg) -> Sub msg
