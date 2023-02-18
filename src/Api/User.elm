module Api.User exposing
    ( User
    , decoder
    , authentication, registration, update
    , getUser
    )

{-|

@docs User
@docs decoder

@docs authentication, registration, current, update

-}

import Api.Token exposing (Token)
import BackendTask exposing (BackendTask)
import BackendTask.Http
import FatalError exposing (FatalError)
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
        "https://api.realworld.io/api/users/login"
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


registration :
    { user :
        { user
            | username : String
            , email : String
            , password : String
        }
    }
    -> BackendTask FatalError (Result (List String) User)
registration options =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Encode.string options.user.username )
                        , ( "email", Encode.string options.user.email )
                        , ( "password", Encode.string options.user.password )
                        ]
                  )
                ]
    in
    Api.Token.requestWithErrors "POST"
        (BackendTask.Http.jsonBody body)
        Nothing
        { url = "https://api.realworld.io/api/users"
        , expect = Json.field "user" decoder
        }


update :
    { token : Token
    , user :
        { user
            | username : String
            , email : String
            , password : Maybe String
            , image : String
            , bio : String
        }
    }
    -> BackendTask FatalError (Result (List String) User)
update options =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "user"
                  , Encode.object
                        (List.concat
                            [ [ ( "username", Encode.string options.user.username )
                              , ( "email", Encode.string options.user.email )
                              , ( "image", Encode.string options.user.image )
                              , ( "bio", Encode.string options.user.bio )
                              ]
                            , case options.user.password of
                                Just password ->
                                    [ ( "password", Encode.string password ) ]

                                Nothing ->
                                    []
                            ]
                        )
                  )
                ]
    in
    Api.Token.put
        (Just options.token)
        { url = "https://api.realworld.io/api/user"
        , body = BackendTask.Http.jsonBody body
        , expect = Json.field "user" decoder
        }
