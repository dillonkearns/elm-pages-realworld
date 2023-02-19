module Route.Logout exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask
import Effect
import ErrorPage
import FatalError
import Form
import Form.Validation
import Head
import Html
import Html.Attributes
import Pages.Msg
import Pages.PageUrl
import Path
import Platform.Sub
import Route
import RouteBuilder
import Server.Request
import Server.Response
import Server.Session
import Server.SetCookie
import Shared
import View


type alias Model =
    {}


type Msg
    = NoOp


type alias RouteParams =
    {}


route : RouteBuilder.StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.buildWithLocalState
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
        (RouteBuilder.serverRender { data = data, action = action, head = head })


init :
    Maybe Pages.PageUrl.PageUrl
    -> Shared.Model
    -> RouteBuilder.StaticPayload Data ActionData RouteParams
    -> ( Model, Effect.Effect Msg )
init pageUrl sharedModel app =
    ( {}, Effect.none )


update :
    Pages.PageUrl.PageUrl
    -> Shared.Model
    -> RouteBuilder.StaticPayload Data ActionData RouteParams
    -> Msg
    -> Model
    -> ( Model, Effect.Effect msg )
update pageUrl sharedModel app msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


subscriptions :
    Maybe Pages.PageUrl.PageUrl
    -> RouteParams
    -> Path.Path
    -> Shared.Model
    -> Model
    -> Sub Msg
subscriptions maybePageUrl routeParams path sharedModel model =
    Platform.Sub.none


type alias Data =
    {}


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    Server.Request.succeed (BackendTask.succeed (Server.Response.render {}))


head : RouteBuilder.StaticPayload Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    Maybe Pages.PageUrl.PageUrl
    -> Shared.Model
    -> Model
    -> RouteBuilder.StaticPayload Data ActionData RouteParams
    -> View.View (Pages.Msg.Msg Msg)
view maybeUrl sharedModel model app =
    { title = "Logout"
    , body =
        [ Html.h2 [] [ Html.text "Form" ]
        , Form.renderHtml
            []
            (\_ -> Nothing)
            app
            ()
            (Form.toDynamicTransition "form" form)
        ]
    }


action :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage))
action routeParams =
    Server.Request.formData formHandlers
        |> Server.Session.withSession
            { name = "realworld-user"
            , secrets = BackendTask.succeed [ "not-so-secret" ]
            , options =
                Server.SetCookie.initOptions
                    |> Server.SetCookie.withPath "/"
            }
            (\formData sessionResult ->
                let
                    okSession : Server.Session.Session
                    okSession =
                        sessionResult
                            |> Result.withDefault Server.Session.empty
                in
                case formData of
                    ( _, Ok _ ) ->
                        BackendTask.succeed
                            -- clear out session to log user out
                            ( Server.Session.empty
                            , Route.redirectTo Route.Login
                            )

                    _ ->
                        BackendTask.succeed ( okSession, Server.Response.render {} )
            )


form : Form.HtmlForm String () input Msg
form =
    { combine = Form.Validation.succeed ()
    , view =
        \formState ->
            [ if formState.isTransitioning then
                Html.button
                    [ Html.Attributes.disabled True ]
                    [ Html.text "Logging out..." ]

              else
                Html.button [] [ Html.text "Log out" ]
            ]
    }
        |> Form.init


formHandlers : Form.ServerForms String ()
formHandlers =
    Form.initCombined identity form
