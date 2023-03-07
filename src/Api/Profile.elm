module Api.Profile exposing
    ( Profile
    , decoder
    , get, follow, unfollow
    )

{-|

@docs Profile
@docs decoder
@docs get, follow, unfollow

-}

import Api.Token exposing (Token)
import BackendTask exposing (BackendTask)
import BackendTask.Http
import FatalError exposing (FatalError)
import Json.Decode as Json
import Utils.Json


type alias Profile =
    { username : String
    , bio : Maybe String
    , image : String
    , following : Bool
    }


decoder : Json.Decoder Profile
decoder =
    Json.map4 Profile
        (Json.field "username" Json.string)
        (Json.field "bio" (Json.maybe Json.string))
        (Json.field "image" (Json.string |> Utils.Json.withDefault "https://static.productionready.io/images/smiley-cyrus.jpg"))
        (Json.field "following" Json.bool)



-- ENDPOINTS


get :
    { token : Maybe Token
    , username : String
    }
    -> BackendTask FatalError Profile
get options =
    Api.Token.get options.token
        { url = "https://api.realworld.io/api/profiles/" ++ options.username
        , expect = Json.field "profile" decoder |> BackendTask.Http.expectJson
        }


follow :
    { token : Token
    , username : String
    }
    -> BackendTask FatalError Profile
follow options =
    Api.Token.post (Just options.token)
        { url = "https://api.realworld.io/api/profiles/" ++ options.username ++ "/follow"
        , body = BackendTask.Http.emptyBody
        , expect = Json.field "profile" decoder |> BackendTask.Http.expectJson
        }


unfollow :
    { token : Token
    , username : String
    }
    -> BackendTask FatalError Profile
unfollow options =
    Api.Token.delete (Just options.token)
        { url = "https://api.realworld.io/api/profiles/" ++ options.username ++ "/follow"
        , expect = Json.field "profile" decoder |> BackendTask.Http.expectJson
        }
