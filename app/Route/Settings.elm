module Route.Settings exposing (ActionData, Data, Model, Msg, RouteParams, route)

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
import Form.Value
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
import Utils.Maybe
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
    { title = "Settings"
    , body =
        [ div [ class "settings-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                        , br [] []
                        , Components.ErrorList.view (app.action |> Maybe.map .errors |> Maybe.withDefault [])
                        , Utils.Maybe.view (app.action |> Maybe.andThen .message) <|
                            \message ->
                                p [ class "text-success" ] [ text message ]
                        , Form.renderHtml []
                            (\_ -> Nothing)
                            app
                            app.data.user
                            (Form.toDynamicTransition "form" form)
                        ]
                    ]
                ]
            ]
        ]
            |> Layout.view (Just app.data.user)
    }



-- ELM-PAGES AND SERVER-SIDE


type alias RouteParams =
    {}


type alias Data =
    { user : User
    }


type alias ActionData =
    { errors : List String
    , message : Maybe String
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
                        case maybeUser of
                            Just user ->
                                Server.Response.render { user = user }

                            Nothing ->
                                Route.redirectTo Route.Login
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
                    ( _, parsedForm ) ->
                        case parsedForm of
                            Ok (Action okForm) ->
                                case token of
                                    Just justToken ->
                                        Api.User.update
                                            { token = justToken
                                            , user = okForm
                                            }
                                            |> BackendTask.map
                                                (\updatedUser ->
                                                    \_ ->
                                                        Server.Response.render
                                                            (case updatedUser of
                                                                Ok _ ->
                                                                    { errors = []
                                                                    , message = Just "User updated!"
                                                                    }

                                                                Err errors ->
                                                                    { errors = errors
                                                                    , message = Just "User updated!"
                                                                    }
                                                            )
                                                )

                                    Nothing ->
                                        BackendTask.succeed
                                            (\_ ->
                                                Route.redirectTo Route.Login
                                            )

                            Err _ ->
                                BackendTask.succeed
                                    (\_ ->
                                        Server.Response.render
                                            { errors = []
                                            , message = Nothing
                                            }
                                    )
            )


form : Form.HtmlForm String ParsedForm User Msg
form =
    (\image username bio email password ->
        { combine =
            ParsedForm
                |> Form.Validation.succeed
                |> Form.Validation.andMap image
                |> Form.Validation.andMap username
                |> Form.Validation.andMap bio
                |> Form.Validation.andMap email
                |> Form.Validation.andMap password
        , view =
            \formState ->
                let
                    fieldView : String -> Form.Validation.Field error parsed Form.FieldView.Input -> Bool -> Html msg
                    fieldView label field large =
                        fieldset [ class "form-group" ]
                            [ Form.FieldView.input
                                [ if large then
                                    class "form-control form-control-lg"

                                  else
                                    class "form-control"
                                , placeholder label
                                ]
                                field
                            ]
                in
                [ fieldset []
                    [ fieldView "URL of profile picture" image False
                    , fieldView "Your Username" username True
                    , fieldView "Short bio about you" bio True
                    , fieldView "Email" email True
                    , fieldView "Password" password True
                    ]
                , if formState.isTransitioning then
                    button
                        [ Html.Attributes.disabled True
                        , class "btn btn-lg btn-primary pull-xs-right"
                        ]
                        [ text "Updating Settings..." ]

                  else
                    button [ class "btn btn-lg btn-primary pull-xs-right" ] [ text "Update Settings" ]
                ]
        }
    )
        |> Form.init
        |> Form.field "image"
            (Form.Field.required "Required" Form.Field.text
                |> Form.Field.withInitialValue (.image >> Form.Value.string)
            )
        |> Form.field "username"
            (Form.Field.required "Required" Form.Field.text
                |> Form.Field.withInitialValue (.username >> Form.Value.string)
            )
        |> Form.field "bio"
            (Form.Field.textarea
                { rows = Just 8, cols = Nothing }
                (Form.Field.required "Required" Form.Field.text
                    |> Form.Field.withOptionalInitialValue (.bio >> Maybe.map Form.Value.string)
                )
            )
        |> Form.field "email"
            (Form.Field.required "Required" Form.Field.text
                |> Form.Field.withInitialValue (.email >> Form.Value.string)
            )
        |> Form.field "password" Form.Field.text


type Action
    = Action ParsedForm


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Action form


type alias ParsedForm =
    { image : String
    , username : String
    , bio : String
    , email : String
    , password : Maybe String
    }
