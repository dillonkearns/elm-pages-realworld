module MySession exposing (withUser)

import Api.Token as Token exposing (Token)
import Api.User exposing (User)
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Server.Request
import Server.Response exposing (Response)
import Server.Session
import Server.SetCookie


withUser :
    ({ parsedRequest : a, token : Maybe Token }
     -> BackendTask FatalError (Maybe User -> Response data errorPage)
    )
    -> Server.Request.Parser a
    -> Server.Request.Parser (BackendTask FatalError (Response data errorPage))
withUser otherTask requestParser =
    requestParser
        |> Server.Session.withSession
            { name = "realworld-user"
            , secrets = BackendTask.succeed [ "not-so-secret" ]
            , options =
                Server.SetCookie.initOptions
                    |> Server.SetCookie.withPath "/"
            }
            (\parsedRequest sessionResult ->
                let
                    okSession : Server.Session.Session
                    okSession =
                        sessionResult
                            |> Result.withDefault Server.Session.empty

                    token : Maybe Token
                    token =
                        okSession |> Token.fromSession
                in
                BackendTask.map2
                    (\maybeUser toRender ->
                        ( okSession
                        , toRender maybeUser
                        )
                    )
                    (Api.User.getUser token)
                    (otherTask
                        { parsedRequest = parsedRequest
                        , token = token
                        }
                    )
            )
