module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import Api.Article.Filters as Filters
import Api.Article.Tag exposing (Tag)
import Api.User exposing (User)
import BackendTask
import Components.ArticleList
import Components.IconButton as IconButton
import Effect
import ErrorPage
import FatalError
import Form
import Form.Field
import Form.Validation
import Form.Value
import Head
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Layout
import MySession
import Pages.Msg
import Pages.PageUrl
import Path
import RouteBuilder
import Server.Request
import Server.Response
import Shared
import Utils.Form exposing (inProgressText)
import Utils.Maybe
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
    Sub.none


type alias Data =
    { tags : List Tag
    , activeTab : Tab
    , page : Int
    , listing : Api.Article.Listing
    , user : Maybe User
    }


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    Server.Request.succeed
        ()
        |> MySession.withUser
            (\{ token } ->
                BackendTask.map2
                    (\tags listing ->
                        \user ->
                            Server.Response.render
                                { tags = tags
                                , activeTab =
                                    -- TODO check query params for active tab
                                    user
                                        |> Maybe.map FeedFor
                                        |> Maybe.withDefault Global
                                , page =
                                    -- TODO get page from query params
                                    1
                                , listing = listing
                                , user = user
                                }
                    )
                    Api.Article.Tag.list
                    (Api.Article.list
                        { page =
                            -- TODO get page from query params
                            1
                        , filters = Filters.create
                        , token = token
                        }
                    )
            )


head : RouteBuilder.StaticPayload Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    Maybe Pages.PageUrl.PageUrl
    -> Shared.Model
    -> Model
    -> RouteBuilder.StaticPayload Data ActionData RouteParams
    -> View.View (Pages.Msg.Msg Msg)
view maybeUrl shared model app =
    { title = ""
    , body =
        [ div [ class "home-page" ]
            [ div [ class "banner" ]
                [ div [ class "container" ]
                    [ h1 [ class "logo-font" ] [ text "conduit" ]
                    , p [] [ text "A place to share your knowledge." ]
                    ]
                ]
            , div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-9" ] <|
                        (viewTabs app.data.user app.data.activeTab
                            :: Components.ArticleList.view
                                { user = app.data.user
                                , articleListing = app.data.listing
                                , toggleFavoriteView = toggleFavoriteView app
                                }
                        )
                    , div [ class "col-md-3" ]
                        [ viewTags app.data.tags
                        ]
                    ]
                ]
            ]
        ]
            |> Layout.view app.data.user
    }


toggleFavoriteView : RouteBuilder.StaticPayload Data ActionData RouteParams -> Article -> Html (Pages.Msg.Msg Msg)
toggleFavoriteView app article =
    Form.renderHtml []
        (\_ -> Nothing)
        app
        article
        (Form.toDynamicFetcher
            (if article.favorited then
                "unfavorite-" ++ article.slug

             else
                "favorite-" ++ article.slug
            )
            favoriteForm
        )


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
                            Ok (Action { slug, setFavorite }) ->
                                case token of
                                    Just justToken ->
                                        (if setFavorite then
                                            Api.Article.favorite

                                         else
                                            Api.Article.unfavorite
                                        )
                                            { token = justToken
                                            , slug = slug
                                            }
                                            |> BackendTask.map
                                                (\_ ->
                                                    \_ ->
                                                        Server.Response.render {}
                                                )

                                    Nothing ->
                                        BackendTask.succeed (\_ -> Server.Response.render {})

                            Err _ ->
                                BackendTask.succeed (\_ -> Server.Response.render {})
            )



-- INIT


type Tab
    = FeedFor User
    | Global
    | TagFilter Tag



--fetchArticlesForTab :
--    Shared.Model
--    ->
--        { model
--            | page : Int
--            , activeTab : Tab
--        }
--    -> Cmd Msg
--fetchArticlesForTab shared model =
--    case model.activeTab of
--        Global ->
--            Api.Article.list
--                { filters = Filters.create
--                , page = model.page
--                , token = Maybe.map .token shared.user
--                , onResponse = GotArticles
--                }
--
--        FeedFor user ->
--            Api.Article.feed
--                { token = user.token
--                , page = model.page
--                , onResponse = GotArticles
--                }
--
--        TagFilter tag ->
--            Api.Article.list
--                { filters =
--                    Filters.create
--                        |> Filters.withTag tag
--                , page = model.page
--                , token = Maybe.map .token shared.user
--                , onResponse = GotArticles
--                }
-- UPDATE
--type Msg
--    = SelectedTab Tab
--    | ClickedPage Int
--update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
--update shared msg model =
--    case msg of
--        SelectedTab tab ->
--            let
--                newModel : Model
--                newModel =
--                    { model
--                        | activeTab = tab
--                        , listing = Api.Data.Loading
--                        , page = 1
--                    }
--            in
--            ( newModel
--            , fetchArticlesForTab shared newModel
--            )
--        ClickedPage page_ ->
--            let
--                newModel : Model
--                newModel =
--                    { model
--                        | listing = Api.Data.Loading
--                        , page = page_
--                    }
--            in
--            ( newModel
--            , fetchArticlesForTab shared newModel
--            )
-- VIEW


viewTabs :
    Maybe User
    -> Tab
    -> Html msg
viewTabs maybeUser activeTab =
    div [ class "feed-toggle" ]
        [ ul [ class "nav nav-pills outline-active" ]
            [ Utils.Maybe.view maybeUser <|
                \user ->
                    li [ class "nav-item" ]
                        [ button
                            [ class "nav-link"
                            , classList [ ( "active", activeTab == FeedFor user ) ]

                            --, Events.onClick (SelectedTab (FeedFor user))
                            ]
                            [ text "Your Feed" ]
                        ]
            , li [ class "nav-item" ]
                [ button
                    [ class "nav-link"
                    , classList [ ( "active", activeTab == Global ) ]

                    --, Events.onClick (SelectedTab Global)
                    ]
                    [ text "Global Feed" ]
                ]
            , case activeTab of
                TagFilter tag ->
                    li [ class "nav-item" ] [ a [ class "nav-link active" ] [ text ("#" ++ tag) ] ]

                _ ->
                    text ""
            ]
        ]


viewTags : List Tag -> Html msg
viewTags tags =
    div [ class "sidebar" ]
        [ p [] [ text "Popular Tags" ]
        , div [ class "tag-list" ] <|
            List.map
                (\tag ->
                    button
                        [ class "tag-pill tag-default"

                        --, Events.onClick (SelectedTab (TagFilter tag))
                        ]
                        [ text tag ]
                )
                tags
        ]


favoriteForm : Form.HtmlForm String FavoriteAction Article Msg
favoriteForm =
    (\slug setFavorite ->
        { combine =
            Form.Validation.succeed FavoriteAction
                |> Form.Validation.andMap slug
                |> Form.Validation.andMap setFavorite
        , view =
            \formState ->
                [ if formState.data.favorited then
                    IconButton.view
                        { color = IconButton.FilledGreen
                        , icon = IconButton.Heart
                        , label = inProgressText formState <| " " ++ String.fromInt formState.data.favoritesCount
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGreen
                        , icon = IconButton.Heart
                        , label = inProgressText formState <| " " ++ String.fromInt formState.data.favoritesCount
                        }
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "slug" (Form.Field.required "Required" Form.Field.text |> Form.Field.withInitialValue (.slug >> Form.Value.string))
        |> Form.hiddenField "set-favorite" (Form.Field.checkbox |> Form.Field.withInitialValue (.favorited >> not >> Form.Value.bool))
        |> Form.hiddenKind ( "kind", "favorite" ) "Expected kind."


type alias FavoriteAction =
    { slug : String
    , setFavorite : Bool
    }


type Action
    = Action FavoriteAction


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Action favoriteForm
