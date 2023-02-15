module Api.Article.Tag exposing (Tag, list)

import BackendTask
import BackendTask.Http
import FatalError exposing (FatalError)
import Json.Decode as Json


type alias Tag =
    String


list : BackendTask.BackendTask FatalError (List String)
list =
    BackendTask.Http.getJson "https://conduit.productionready.io/api/tags"
        (Json.field "tags" (Json.list Json.string))
        |> BackendTask.allowFatal
