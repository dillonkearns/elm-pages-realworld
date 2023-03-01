module Route.Register exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.User exposing (User)
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
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Layout
import MySession
import Pages.PageUrl
import PagesMsg exposing (PagesMsg)
import Path
import Route
import RouteBuilder
import Server.Request
import Server.Response
import Shared
import View


route : RouteBuilder.StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.buildWithLocalState
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
        (RouteBuilder.serverRender { data = data, action = action, head = head })



-- INIT


type alias Model =
    {}


init :
    Maybe Pages.PageUrl.PageUrl
    -> Shared.Model
    -> RouteBuilder.StaticPayload Data ActionData RouteParams
    -> ( Model, Effect.Effect Msg )
init pageUrl sharedModel app =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = NoOp


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
    Sub.none



-- VIEW


view :
    Maybe Pages.PageUrl.PageUrl
    -> Shared.Model
    -> Model
    -> RouteBuilder.StaticPayload Data ActionData RouteParams
    -> View.View (PagesMsg Msg)
view maybeUrl sharedModel model app =
    { title = "Sign up"
    , body =
        [ div [ class "auth-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Sign up" ]
                        , p [ class "text-xs-center" ]
                            [ Route.Login |> Route.link [] [ text "Have an account?" ]
                            ]
                        , case app.action of
                            Just _ ->
                                let
                                    reasons =
                                        []
                                in
                                Components.ErrorList.view reasons

                            _ ->
                                text ""
                        , Form.renderHtml [] (\_ -> Nothing) app () (Form.toDynamicTransition "form" form)
                        ]
                    ]
                ]
            ]
        ]
            |> Layout.view app.data.user
    }



-- ELM-PAGES


type alias RouteParams =
    {}


type alias Data =
    { user : Maybe User
    }


type alias ActionData =
    { errors : List String
    }


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    Server.Request.succeed ()
        |> MySession.withUser
            (\_ ->
                BackendTask.succeed
                    (\maybeUser ->
                        Server.Response.render { user = maybeUser }
                    )
            )


head : RouteBuilder.StaticPayload Data ActionData RouteParams -> List Head.Tag
head app =
    []


action :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage))
action routeParams =
    Server.Request.map
        (\( _, parsedForm ) ->
            case parsedForm of
                Ok (Action okForm) ->
                    Api.User.registration
                        { user = okForm
                        }
                        |> BackendTask.map
                            (\updatedUser ->
                                case updatedUser of
                                    Ok _ ->
                                        Route.redirectTo Route.Index

                                    Err errors ->
                                        Server.Response.render
                                            { errors = errors
                                            }
                            )

                Err _ ->
                    Server.Response.render { errors = [] }
                        |> BackendTask.succeed
        )
        (Server.Request.formData formHandlers)


form : Form.HtmlForm String ParsedForm input Msg
form =
    (\username email password ->
        { combine =
            ParsedForm
                |> Form.Validation.succeed
                |> Form.Validation.andMap username
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
                [ fieldView "Your Name" username
                , fieldView "Email" email
                , fieldView "Password" password
                , if formState.isTransitioning then
                    button
                        [ Html.Attributes.disabled True
                        , class "btn btn-lg btn-primary pull-xs-right"
                        ]
                        [ text "Signing up..." ]

                  else
                    button [ class "btn btn-lg btn-primary pull-xs-right" ] [ text "Sign up" ]
                ]
        }
    )
        |> Form.init
        |> Form.field "username" (Form.Field.required "Required" Form.Field.text)
        |> Form.field "email" (Form.Field.required "Required" Form.Field.text)
        |> Form.field "password" (Form.Field.required "Required" Form.Field.text |> Form.Field.password)


type Action
    = Action ParsedForm


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Action form


type alias ParsedForm =
    { username : String, email : String, password : String }
