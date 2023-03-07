module Route.Login exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Token as Token
import Api.User
import BackendTask
import Components.ErrorList
import Effect
import ErrorPage
import FatalError
import Form
import Form.Field
import Form.FieldView
import Form.Validation
import Head
import Html exposing (button, div, fieldset, h1, p, text)
import Html.Attributes exposing (class, placeholder)
import Layout
import Pages.PageUrl
import PagesMsg exposing (PagesMsg)
import Path
import Platform.Sub
import Result
import Route
import RouteBuilder exposing (App)
import Server.Request
import Server.Response
import Server.Session
import Server.SetCookie
import Shared
import Utils.Form exposing (inProgressText)
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
    App Data ActionData RouteParams
    -> Shared.Model
    -> ( Model, Effect.Effect Msg )
init app shared =
    ( {}, Effect.none )


update :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect.Effect msg )
update app shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


subscriptions :
    RouteParams
    -> Path.Path
    -> Shared.Model
    -> Model
    -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none


type alias Data =
    {}


type alias ActionData =
    { errors : List String }


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    Server.Request.succeed (BackendTask.succeed (Server.Response.render {}))


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View.View (PagesMsg Msg)
view app shared model =
    { title = "Sign in"
    , body =
        [ div [ class "auth-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ]
                            [ text "Sign in"
                            ]
                        , p [ class "text-xs-center" ]
                            [ Route.Register |> Route.link [] [ text "Need an account?" ]
                            ]
                        , case app.action |> Maybe.map .errors |> Maybe.withDefault [] of
                            [] ->
                                text ""

                            reasons ->
                                Components.ErrorList.view reasons
                        , Form.renderHtml "form" [] (\_ -> Nothing) app () form
                        ]
                    ]
                ]
            ]
        ]
            |> Layout.view Nothing
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
                    ( formResponse, parsedForm ) ->
                        case parsedForm of
                            Ok (Action okForm) ->
                                Api.User.authentication
                                    { email = okForm.email
                                    , password = okForm.password
                                    }
                                    |> BackendTask.map
                                        (\userResult ->
                                            case userResult of
                                                Ok okUser ->
                                                    ( okSession
                                                        |> Server.Session.insert "token" (Token.toString okUser.token)
                                                    , Route.redirectTo Route.Index
                                                    )

                                                Err errors ->
                                                    ( okSession
                                                    , Server.Response.render { errors = errors }
                                                    )
                                        )

                            Err _ ->
                                BackendTask.succeed
                                    ( okSession
                                    , Server.Response.render { errors = [] }
                                    )
            )


form : Form.HtmlForm String ParsedForm () Msg
form =
    (\email password ->
        { combine =
            ParsedForm
                |> Form.Validation.succeed
                |> Form.Validation.andMap email
                |> Form.Validation.andMap password
        , view =
            \formState ->
                let
                    fieldView label field =
                        fieldset [ class "form-group" ]
                            [ Form.FieldView.input
                                [ class "form-control form-control-lg"
                                , placeholder label
                                ]
                                field
                            ]
                in
                [ fieldView "Email" email
                , fieldView "Password" password
                , button [ class "btn btn-lg btn-primary pull-xs-right" ]
                    [ text <| inProgressText formState "Sign in"
                    ]
                ]
        }
    )
        |> Form.init
        |> Form.field "email" (Form.Field.required "Required" Form.Field.text)
        |> Form.field "password" (Form.Field.required "Required" Form.Field.text |> Form.Field.password)


type Action
    = Action ParsedForm


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Action form


type alias ParsedForm =
    { email : String, password : String }
