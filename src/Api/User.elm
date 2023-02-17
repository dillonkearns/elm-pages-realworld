module Api.User exposing
    ( User
    , decoder, encode
    , authentication
    ,  getUser
       --, authentication, registration, update

    )

{-|

@docs User
@docs decoder, encode

@docs authentication, registration, current, update

-}

import Api.Data exposing (Data)
import Api.Token exposing (Token)
import BackendTask exposing (BackendTask)
import BackendTask.Http
import FatalError exposing (FatalError)
import Http
import Json.Decode as Json
import Json.Encode as Encode
import Utils.Json


type alias User =
    { email : String
    , token : Token
    , username : String
    , bio : Maybe String
    , image : String
    }


decoder : Json.Decoder User
decoder =
    Json.map5 User
        (Json.field "email" Json.string)
        (Json.field "token" Api.Token.decoder)
        (Json.field "username" Json.string)
        (Json.field "bio" (Json.maybe Json.string))
        (Json.field "image" (Json.string |> Utils.Json.withDefault "https://static.productionready.io/images/smiley-cyrus.jpg"))


encode : User -> Json.Value
encode user =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "email", Encode.string user.email )
        , ( "token", Api.Token.encode user.token )
        , ( "image", Encode.string user.image )
        , ( "bio", Utils.Json.maybe Encode.string user.bio )
        ]


authentication :
    { email : String, password : String }
    -> BackendTask FatalError User
authentication user =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "email", Encode.string user.email )
                        , ( "password", Encode.string user.password )
                        ]
                  )
                ]
    in
    BackendTask.Http.post
        "https://conduit.productionready.io/api/users/login"
        (BackendTask.Http.jsonBody body)
        (BackendTask.Http.expectJson
            (Json.field "user" decoder)
        )
        |> BackendTask.allowFatal


getUser : Maybe Token -> BackendTask FatalError (Maybe User)
getUser maybeToken =
    case maybeToken of
        Just token ->
            Api.Token.get (Just token)
                { url = "https://api.realworld.io/api/user"
                , expect = Json.field "user" decoder
                }
                |> BackendTask.map Just

        Nothing ->
            BackendTask.succeed Nothing



--
--
--registration :
--    { user :
--        { user
--            | username : String
--            , email : String
--            , password : String
--        }
--    , onResponse : Data User -> msg
--    }
--    -> Cmd msg
--registration options =
--    let
--        body : Json.Value
--        body =
--            Encode.object
--                [ ( "user"
--                  , Encode.object
--                        [ ( "username", Encode.string options.user.username )
--                        , ( "email", Encode.string options.user.email )
--                        , ( "password", Encode.string options.user.password )
--                        ]
--                  )
--                ]
--    in
--    Http.post
--        { url = "https://conduit.productionready.io/api/users"
--        , body = Http.jsonBody body
--        , expect =
--            Api.Data.expectJson options.onResponse
--                (Json.field "user" decoder)
--        }
--
--
--update :
--    { token : Token
--    , user :
--        { user
--            | username : String
--            , email : String
--            , password : Maybe String
--            , image : String
--            , bio : String
--        }
--    , onResponse : Data User -> msg
--    }
--    -> Cmd msg
--update options =
--    let
--        body : Json.Value
--        body =
--            Encode.object
--                [ ( "user"
--                  , Encode.object
--                        (List.concat
--                            [ [ ( "username", Encode.string options.user.username )
--                              , ( "email", Encode.string options.user.email )
--                              , ( "image", Encode.string options.user.image )
--                              , ( "bio", Encode.string options.user.bio )
--                              ]
--                            , case options.user.password of
--                                Just password ->
--                                    [ ( "password", Encode.string password ) ]
--
--                                Nothing ->
--                                    []
--                            ]
--                        )
--                  )
--                ]
--    in
--    Api.Token.put (Just options.token)
--        { url = "https://conduit.productionready.io/api/user"
--        , body = Http.jsonBody body
--        , expect =
--            Api.Data.expectJson options.onResponse
--                (Json.field "user" decoder)
--        }
