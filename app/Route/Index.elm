module Route.Index exposing (ActionData, Data, route, RouteParams, Msg, Model)

{-|

@docs ActionData, Data, route, RouteParams, Msg, Model

-}

import Api.Article exposing (Article)
import Api.Article.Filters as Filters
import Api.Article.Tag exposing (Tag)
import Api.Token as Token exposing (Token)
import Api.User exposing (User)
import BackendTask
import Components.ArticleList
import Effect
import ErrorPage
import FatalError
import Head
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Pages.Msg
import Pages.PageUrl
import Path
import Platform.Sub
import RouteBuilder
import Server.Request
import Server.Response
import Server.Session
import Server.SetCookie
import Shared
import Utils.Maybe
import View exposing (View)


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
    { tags : List Tag
    , activeTab : Tab
    , page : Int
    , listing : Api.Article.Listing
    }


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    -- -- TODO adapt this logic for elm-pages
    --        activeTab : Tab
    --        activeTab =
    --            shared.user
    --                |> Maybe.map FeedFor
    --                |> Maybe.withDefault Global
    Server.Request.succeed
        ()
        |> Server.Session.withSession
            { name = "realworld-user"
            , secrets = BackendTask.succeed [ "not-so-secret" ]
            , options =
                Server.SetCookie.initOptions
                    |> Server.SetCookie.withPath "/"
            }
            (\() sessionResult ->
                let
                    okSession : Server.Session.Session
                    okSession =
                        sessionResult
                            |> Result.withDefault Server.Session.empty
                            |> Debug.log "session"

                    token : Maybe Token
                    token =
                        okSession |> Token.fromSession
                in
                BackendTask.map2
                    (\tags listing ->
                        ( okSession
                        , Server.Response.render
                            { tags = tags
                            , activeTab =
                                -- TODO check query params for active tab
                                Global
                            , page =
                                -- TODO get page from query params
                                1
                            , listing = listing
                            }
                        )
                    )
                    Api.Article.Tag.list
                    (Api.Article.list
                        { page = 1
                        , filters = Filters.create
                        , token = token |> Debug.log "token"
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
                        (viewTabs shared app.data.activeTab
                            :: Components.ArticleList.view
                                { user = shared.user
                                , articleListing = app.data.listing
                                }
                        )
                    , div [ class "col-md-3" ]
                        [ viewTags app.data.tags
                        ]
                    ]
                ]
            ]
        ]
    }


action :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response ActionData ErrorPage.ErrorPage))
action routeParams =
    Server.Request.succeed (BackendTask.succeed (Server.Response.render {}))



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
--    | ClickedFavorite User Article
--    | ClickedUnfavorite User Article
--    | ClickedPage Int
--    | UpdatedArticle (Data Article)
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
--
--        ClickedFavorite user article ->
--            ( model
--            , Api.Article.favorite
--                { token = user.token
--                , slug = article.slug
--                , onResponse = UpdatedArticle
--                }
--            )
--
--        ClickedUnfavorite user article ->
--            ( model
--            , Api.Article.unfavorite
--                { token = user.token
--                , slug = article.slug
--                , onResponse = UpdatedArticle
--                }
--            )
--
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
--
--        UpdatedArticle (Api.Data.Success article) ->
--            ( { model
--                | listing =
--                    Api.Data.map (Api.Article.updateArticle article)
--                        model.listing
--              }
--            , Cmd.none
--            )
--
--        UpdatedArticle _ ->
--            ( model, Cmd.none )
--
--
-- VIEW


viewTabs :
    Shared.Model
    -> Tab
    -> Html msg
viewTabs shared activeTab =
    div [ class "feed-toggle" ]
        [ ul [ class "nav nav-pills outline-active" ]
            [ Utils.Maybe.view shared.user <|
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
