module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import Api.Article.Filters as Filters exposing (Filters)
import Api.Article.Tag exposing (Tag)
import Api.User exposing (User)
import BackendTask
import Browser.Dom as Dom
import Components.ArticleList
import Components.IconButton as IconButton
import Effect exposing (Effect)
import ErrorPage
import FatalError
import Form
import Form.Field
import Form.FieldView
import Form.Validation
import Form.Value
import Head
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Layout
import MySession
import PagesMsg exposing (PagesMsg)
import Path
import Route
import RouteBuilder exposing (App)
import Server.Request
import Server.Response
import Shared
import Task exposing (Task)
import Utils.Form exposing (inProgressText)
import Utils.Maybe
import View


type alias Model =
    {}


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


subscriptions :
    RouteParams
    -> Path.Path
    -> Shared.Model
    -> Model
    -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none


type alias Data =
    { tags : List Tag
    , activeTab : Tab
    , page : Int
    , filters : Filters
    , listing : Api.Article.Listing
    , user : Maybe User
    }


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Parser (BackendTask.BackendTask FatalError.FatalError (Server.Response.Response Data ErrorPage.ErrorPage))
data routeParams =
    Server.Request.formData (Form.initCombined identity filtersForm)
        |> MySession.withUser
            (\{ token, parsedRequest } ->
                let
                    ( page, maybeTag, useGlobal ) =
                        case parsedRequest of
                            ( _, Ok parsedFilters ) ->
                                ( parsedFilters.page |> Maybe.withDefault 1
                                , parsedFilters.tag
                                , parsedFilters.page /= Nothing || parsedFilters.globalTab
                                )

                            _ ->
                                ( 1, Nothing, False )
                in
                BackendTask.map2
                    (\tags listing ->
                        \user ->
                            Server.Response.render
                                { tags = tags
                                , activeTab =
                                    case maybeTag of
                                        Just tag ->
                                            TagFilter tag

                                        Nothing ->
                                            if useGlobal then
                                                Global

                                            else
                                                user
                                                    |> Maybe.map FeedFor
                                                    |> Maybe.withDefault Global
                                , page = page
                                , listing = listing
                                , filters =
                                    Filters.create
                                        |> applyMaybe maybeTag Filters.withTag
                                , user = user
                                }
                    )
                    Api.Article.Tag.list
                    (case ( useGlobal, token, maybeTag ) of
                        ( False, Just justToken, Nothing ) ->
                            Api.Article.feed
                                { page = page
                                , token = justToken
                                }

                        _ ->
                            Api.Article.list
                                { page = page
                                , filters =
                                    Filters.create
                                        |> applyMaybe maybeTag Filters.withTag
                                , token = token
                                }
                    )
            )


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    []


view :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Model
    -> View.View (PagesMsg Msg)
view app shared model =
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
                                , paginationView =
                                    filtersForm
                                        |> Form.withGetMethod
                                        |> Form.withOnSubmit (\_ -> ScrollToTop)
                                        |> Form.renderHtml "filters" [] (\_ -> Nothing) app ( RenderPages, app.data.listing )
                                }
                        )
                    , div [ class "col-md-3" ]
                        [ filtersForm
                            |> Form.withGetMethod
                            |> Form.withOnSubmit (\_ -> ScrollToTop)
                            |> Form.renderHtml "filters" [] (\_ -> Nothing) app ( RenderTags app.data.tags, app.data.listing )
                        ]
                    ]
                ]
            ]
        ]
            |> Layout.view app.data.user
    }


toggleFavoriteView : App Data ActionData RouteParams -> Article -> Html (PagesMsg Msg)
toggleFavoriteView app article =
    Form.renderHtml
        (if article.favorited then
            "unfavorite-" ++ article.slug

         else
            "favorite-" ++ article.slug
        )
        []
        (\_ -> Nothing)
        app
        article
        (favoriteForm
            |> Form.toDynamicFetcher
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
                                                (\_ _ -> Server.Response.render {})

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



-- UPDATE


type Msg
    = NoOp
    | ScrollToTop


update :
    App Data ActionData RouteParams
    -> Shared.Model
    -> Msg
    -> Model
    -> ( Model, Effect Msg )
update app shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        ScrollToTop ->
            ( model
            , Effect.fromCmd (scrollToTop |> Task.perform (\_ -> NoOp))
            )


scrollToTop : Task x ()
scrollToTop =
    Dom.setViewport 0 0 |> Task.onError (\_ -> Task.succeed ())



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
                        [ Route.Index
                            |> Route.link
                                [ class "nav-link"
                                , classList [ ( "active", activeTab == FeedFor user ) ]
                                ]
                                [ text "Your Feed" ]
                        ]
            , li [ class "nav-item" ]
                [ a
                    [ class "nav-link"
                    , classList [ ( "active", activeTab == Global ) ]
                    , href "/?tab=global"
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


applyMaybe : Maybe a -> (a -> chain -> chain) -> chain -> chain
applyMaybe maybe apply chain =
    case maybe of
        Just just ->
            apply just chain

        Nothing ->
            chain


type RenderMode
    = RenderPages
    | RenderTags (List Tag)


type alias ParsedFilters =
    { globalTab : Bool
    , tag : Maybe String
    , page : Maybe Int
    }


filtersForm : Form.HtmlForm String ParsedFilters ( RenderMode, Api.Article.Listing ) Msg
filtersForm =
    (\tab tag pageNumber ->
        { combine =
            Form.Validation.succeed
                ParsedFilters
                |> Form.Validation.andMap
                    (tab
                        |> Form.Validation.map (\tabString -> tabString == Just "global")
                    )
                |> Form.Validation.andMap tag
                |> Form.Validation.andMap pageNumber
        , view =
            \formState ->
                let
                    listing : Api.Article.Listing
                    listing =
                        Tuple.second formState.data

                    viewPage : Int -> Html msg
                    viewPage page =
                        li
                            [ class "page-item"
                            , classList [ ( "active", listing.page == page ) ]
                            ]
                            [ pageNumber
                                |> Form.FieldView.valueButton (String.fromInt page)
                                    [ class "page-link"
                                    ]
                                    [ text (String.fromInt page) ]
                            ]
                in
                case Tuple.first formState.data of
                    RenderPages ->
                        [ List.range 1 listing.totalPages
                            |> List.map viewPage
                            |> ul [ class "pagination" ]
                        ]

                    RenderTags tags ->
                        [ div [ class "sidebar" ]
                            [ p [] [ text "Popular Tags" ]
                            , div [ class "tag-list" ] <|
                                List.map
                                    (\tagText ->
                                        tag
                                            |> Form.FieldView.valueButton tagText
                                                [ class "tag-pill tag-default" ]
                                                [ text tagText ]
                                    )
                                    tags
                            ]
                        ]
        }
    )
        |> Form.init
        |> Form.field "tab" Form.Field.text
        |> Form.field "tag" Form.Field.text
        |> Form.field "page"
            (Form.Field.int { invalid = \_ -> "Expected int." }
                |> Form.Field.withInitialValue (Tuple.second >> .page >> Form.Value.int)
            )


type alias FavoriteAction =
    { slug : String
    , setFavorite : Bool
    }


type Action
    = Action FavoriteAction


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Action favoriteForm
