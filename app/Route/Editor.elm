module Route.Editor exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import BackendTask
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
import MySession
import Pages.Msg
import Pages.PageUrl
import Path
import Platform.Sub
import Route
import RouteBuilder
import Server.Request
import Server.Response
import Shared
import View exposing (View)


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


type alias Data =
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
    -> View.View (Pages.Msg.Msg Msg)
view maybeUrl sharedModel model app =
    { title = "New Article"
    , body =
        [ editorView app
        ]
    }


editorView app =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ] [ text "New Article" ]
                    , br [] []
                    , Form.renderHtml
                        []
                        (\_ -> Nothing)
                        app
                        { errors = app.action |> Maybe.map .errors |> Maybe.withDefault [] }
                        (Form.toDynamicTransition "form" form)
                    ]
                ]
            ]
        ]


type alias RouteParams =
    {}


type alias ActionData =
    { errors : List String
    }


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    Server.Request.succeed
        (BackendTask.succeed
            (Server.Response.render
                {}
            )
        )


head : RouteBuilder.StaticPayload Data ActionData RouteParams -> List Head.Tag
head app =
    []


action :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage))
action routeParams =
    Server.Request.formData formHandlers
        |> MySession.withUser
            (\{ token, parsedRequest } ->
                case parsedRequest of
                    ( formResponse, parsedForm ) ->
                        case parsedForm of
                            Ok (Action okForm) ->
                                case token of
                                    Just justToken ->
                                        BackendTask.map
                                            (\articleResult _ ->
                                                case articleResult of
                                                    Ok newArticle ->
                                                        Route.redirectTo
                                                            (Route.Article__Slug_ { slug = newArticle.slug })

                                                    Err errors ->
                                                        Server.Response.render { errors = errors }
                                            )
                                            (Api.Article.create
                                                { token = justToken
                                                , article =
                                                    { title = okForm.title
                                                    , description = okForm.description
                                                    , body = okForm.body
                                                    , tags =
                                                        okForm.tags
                                                            |> Maybe.withDefault ""
                                                            |> String.split ","
                                                            |> List.map String.trim
                                                    }
                                                }
                                            )

                                    Nothing ->
                                        BackendTask.succeed (\_ -> Route.redirectTo Route.Login)

                            Err _ ->
                                BackendTask.succeed
                                    (\_ ->
                                        Server.Response.render
                                            { errors = [] }
                                    )
            )


form : Form.HtmlForm String ParsedForm { errors : List String } Msg
form =
    (\title description body tags ->
        { combine =
            ParsedForm
                |> Form.Validation.succeed
                |> Form.Validation.andMap title
                |> Form.Validation.andMap description
                |> Form.Validation.andMap body
                |> Form.Validation.andMap tags
        , view =
            \formState ->
                let
                    fieldView label field large =
                        --Html.div
                        --    []
                        --    [ Html.label
                        --        []
                        --        [ Html.text (label ++ " ")
                        --        , Form.FieldView.input [] field
                        --        , errorsView formState.errors field
                        --        ]
                        --    ]
                        fieldset [ class "form-group" ]
                            [ field
                                |> Form.FieldView.input
                                    [ if large then
                                        class "form-control form-control-lg"

                                      else
                                        class "form-control"
                                    , placeholder label
                                    ]
                            ]
                in
                [ fieldset []
                    [ fieldView "Article Title" title True
                    , fieldView "What's this article about?" description False
                    , fieldView "Write your article (in markdown)" body False
                    , fieldView "Enter tags (separated by commas)" tags False
                    , if formState.isTransitioning then
                        button
                            [ class "btn btn-lg pull-xs-right btn-primary"
                            , Html.Attributes.disabled True
                            ]
                            [ text "Publishing..." ]

                      else
                        button
                            [ class "btn btn-lg pull-xs-right btn-primary"
                            ]
                            [ text "Publish" ]
                    ]
                , case formState.data.errors of
                    [] ->
                        text ""

                    reasons ->
                        ul [ class "error-messages" ]
                            (List.map (\message -> li [] [ text message ]) reasons)
                ]
        }
    )
        |> Form.init
        |> Form.field "title" (Form.Field.required "Required" Form.Field.text)
        |> Form.field "description" (Form.Field.required "Required" Form.Field.text)
        |> Form.field "body"
            (Form.Field.textarea
                { rows = Just 8, cols = Nothing }
                (Form.Field.required "Required" Form.Field.text)
            )
        |> Form.field "tags" Form.Field.text


type Action
    = Action ParsedForm


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Action form


type alias ParsedForm =
    { title : String
    , description : String
    , body : String
    , tags : Maybe String
    }
