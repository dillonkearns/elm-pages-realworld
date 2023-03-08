module Route.Editor.Slug__ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import Api.User exposing (User)
import BackendTask
import Effect
import ErrorPage
import FatalError
import Form
import Form.Field
import Form.FieldView
import Form.Validation
import Form.Value
import Head
import Html exposing (..)
import Html.Attributes exposing (class, placeholder)
import Layout
import MySession
import PagesMsg exposing (PagesMsg)
import Path
import Route
import RouteBuilder exposing (App)
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


type alias Data =
    { article : Maybe Article
    , user : User
    }


init :
    App Data ActionData RouteParams
    -> Shared.Model
    -> ( Model, Effect.Effect Msg )
init app shared =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = NoOp


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



-- VIEW


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View.View (PagesMsg Msg)
view app model shared =
    { title =
        case app.routeParams.slug of
            Just _ ->
                "Editing Article"

            Nothing ->
                "New Article"
    , body =
        [ editorView app
        ]
            |> Layout.view app (Just app.data.user)
    }


editorView app =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                    [ h1 [ class "text-xs-center" ]
                        [ text
                            (case app.routeParams.slug of
                                Just _ ->
                                    "Edit Article"

                                Nothing ->
                                    "New Article"
                            )
                        ]
                    , br [] []
                    , Form.renderHtml "form"
                        []
                        (\_ -> Nothing)
                        app
                        { errors = app.action |> Maybe.map .errors |> Maybe.withDefault []
                        , article = app.data.article
                        }
                        form
                    ]
                ]
            ]
        ]


type alias RouteParams =
    { slug : Maybe String
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
            (\{ token } ->
                case token of
                    Just justToken ->
                        routeParams.slug
                            |> Maybe.map
                                (\slug ->
                                    Api.Article.get
                                        { token = Just justToken
                                        , slug = slug
                                        }
                                        |> BackendTask.map Just
                                )
                            |> Maybe.withDefault (BackendTask.succeed Nothing)
                            |> BackendTask.map
                                (\article maybeUser ->
                                    case maybeUser of
                                        Just user ->
                                            Server.Response.render
                                                { article = article
                                                , user = user
                                                }

                                        Nothing ->
                                            Route.redirectTo Route.Login
                                )

                    _ ->
                        BackendTask.succeed
                            (\_ -> Route.redirectTo Route.Login)
            )


head : App Data ActionData RouteParams -> List Head.Tag
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
                    ( _, parsedForm ) ->
                        case parsedForm of
                            Ok (CreateOrUpdateArticle okForm) ->
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
                                            ((case routeParams.slug of
                                                Just slug ->
                                                    Api.Article.update { slug = slug }

                                                Nothing ->
                                                    Api.Article.create
                                             )
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


form :
    Form.HtmlForm
        String
        ArticleForm
        { article : Maybe Article
        , errors : List String
        }
        Msg
form =
    (\title description body tags ->
        { combine =
            ArticleForm
                |> Form.Validation.succeed
                |> Form.Validation.andMap title
                |> Form.Validation.andMap description
                |> Form.Validation.andMap body
                |> Form.Validation.andMap tags
        , view =
            \formState ->
                let
                    fieldView label field large =
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
                            [ text
                                (case formState.data.article of
                                    Just _ ->
                                        "Saving..."

                                    _ ->
                                        "Publishing..."
                                )
                            ]

                      else
                        button
                            [ class "btn btn-lg pull-xs-right btn-primary"
                            ]
                            [ text
                                (case formState.data.article of
                                    Just _ ->
                                        "Save"

                                    _ ->
                                        "Publish"
                                )
                            ]
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
        |> Form.field "title"
            (Form.Field.required "Required" Form.Field.text
                |> Form.Field.withOptionalInitialValue (.article >> Maybe.map .title >> Maybe.map Form.Value.string)
            )
        |> Form.field "description"
            (Form.Field.required "Required" Form.Field.text
                |> Form.Field.withOptionalInitialValue (.article >> Maybe.map .description >> Maybe.map Form.Value.string)
            )
        |> Form.field "body"
            (Form.Field.textarea
                { rows = Just 8, cols = Nothing }
                (Form.Field.required "Required" Form.Field.text)
                |> Form.Field.withOptionalInitialValue (.article >> Maybe.map .body >> Maybe.map Form.Value.string)
            )
        |> Form.field "tags"
            (Form.Field.text
                |> Form.Field.withOptionalInitialValue (.article >> Maybe.map .tags >> Maybe.map (String.join ", ") >> Maybe.map Form.Value.string)
            )


type Action
    = CreateOrUpdateArticle ArticleForm


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined CreateOrUpdateArticle form


type alias ArticleForm =
    { title : String
    , description : String
    , body : String
    , tags : Maybe String
    }
