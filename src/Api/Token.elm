module Api.Token exposing
    ( Token
    , decoder, encode
    , get, put, post, delete
    , fromSession, toString
    )

{-|

@docs Token
@docs decoder, encode
@docs get, put, post, delete

-}

import BackendTask exposing (BackendTask)
import BackendTask.Http
import FatalError exposing (FatalError)
import Json.Decode as Json
import Json.Encode as Encode
import Server.Session as Session exposing (Session)


type Token
    = Token String


toString : Token -> String
toString (Token token) =
    token


fromSession : Session -> Maybe Token
fromSession session =
    session
        |> Session.get "token"
        |> Maybe.map Token


decoder : Json.Decoder Token
decoder =
    Json.map Token Json.string


encode : Token -> Json.Value
encode (Token token) =
    Encode.string token



-- HTTP HELPERS


get :
    Maybe Token
    ->
        { url : String
        , expect : Json.Decoder value
        }
    -> BackendTask FatalError value
get =
    request "GET" BackendTask.Http.emptyBody


delete :
    Maybe Token
    ->
        { url : String
        , expect : Json.Decoder value
        }
    -> BackendTask FatalError value
delete =
    request "DELETE" BackendTask.Http.emptyBody


post :
    Maybe Token
    ->
        { url : String
        , body : BackendTask.Http.Body
        , expect : Json.Decoder value
        }
    -> BackendTask FatalError value
post token options =
    request "POST" options.body token options


put :
    Maybe Token
    ->
        { url : String
        , body : BackendTask.Http.Body
        , expect : Json.Decoder value
        }
    -> BackendTask FatalError value
put token options =
    request "PUT" options.body token options


request :
    String
    -> BackendTask.Http.Body
    -> Maybe Token
    ->
        { options
            | url : String
            , expect : Json.Decoder value
        }
    -> BackendTask FatalError value
request method body maybeToken options =
    BackendTask.Http.request
        { method = method
        , headers =
            case maybeToken of
                Just (Token token) ->
                    [ ( "Authorization", "Token " ++ token ) ]

                Nothing ->
                    []
        , url = options.url
        , body = body
        , timeoutInMs = Just (1000 * 60) -- 60 second timeout
        , retries = Nothing
        }
        (BackendTask.Http.expectJson options.expect)
        |> BackendTask.allowFatal
