module Route.Login exposing (ActionData, Data, route, RouteParams, Msg, Model)

{-|

@docs ActionData, Data, route, RouteParams, Msg, Model

-}

import Api.Token as Token
import Api.User
import BackendTask
import Debug
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
import Pages.Msg
import Pages.PageUrl
import Pages.Script
import Path
import Platform.Sub
import Result
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
    { errors : Form.Response String }


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
    { title = "Sign in"
    , body =
        [ Form.renderHtml
            []
            (\renderHtmlUnpack -> Just renderHtmlUnpack.errors)
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
                    ( formResponse, parsedForm ) ->
                        case parsedForm of
                            Ok (Action okForm) ->
                                Api.User.authentication
                                    { email = okForm.email
                                    , password = okForm.password
                                    }
                                    |> BackendTask.map
                                        (\userResult ->
                                            ( okSession
                                                |> Server.Session.insert "token" (Token.toString userResult.token)
                                                |> Debug.log "inserting session"
                                            , Route.redirectTo Route.Index
                                            )
                                        )

                            Err _ ->
                                BackendTask.map
                                    (\_ ->
                                        ( okSession
                                        , Server.Response.render { errors = formResponse }
                                        )
                                    )
                                    (Pages.Script.log
                                        (Debug.toString parsedForm)
                                    )
            )


errorsView :
    Form.Errors String
    -> Form.Validation.Field String parsed kind
    -> Html.Html (Pages.Msg.Msg Msg)
errorsView errors field =
    if List.isEmpty (Form.errorsForField field errors) then
        Html.div [] []

    else
        Html.div
            []
            [ Html.ul
                []
                (List.map
                    (\error ->
                        Html.li
                            [ Html.Attributes.style "color" "red" ]
                            [ Html.text error ]
                    )
                    (Form.errorsForField field errors)
                )
            ]


form : Form.HtmlForm String ParsedForm input Msg
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
                            , errorsView formState.errors field
                            ]
                in
                [ --, if formState.isTransitioning then
                  --    Html.button
                  --        [ Html.Attributes.disabled True ]
                  --        [ Html.text "Submitting..." ]
                  --
                  --  else
                  --    Html.button [] [ Html.text "Submit" ]
                  --]
                  div [ class "auth-page" ]
                    [ div [ class "container page" ]
                        [ div [ class "row" ]
                            [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                                [ h1 [ class "text-xs-center" ]
                                    [ text "Sign in" --text options.label
                                    ]
                                , p [ class "text-xs-center" ]
                                    [ Route.Register |> Route.link [] [ text "Need an account?" ]
                                    ]

                                --,
                                --case options.user of
                                --    Api.Data.Failure reasons ->
                                --        Components.ErrorList.view reasons
                                --
                                --    _ ->
                                --        text ""
                                --, --form [ Events.onSubmit options.onFormSubmit ] <|
                                --List.concat
                                --    [ List.map viewField options.fields
                                --    , []
                                --    ]
                                , fieldView "Email" email
                                , fieldView "Password" password
                                , button [ class "btn btn-lg btn-primary pull-xs-right" ]
                                    [ text "Sign in" --text options.label
                                    ]
                                ]
                            ]
                        ]
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
