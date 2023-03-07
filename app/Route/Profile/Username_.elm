module Route.Profile.Username_ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import Api.Article.Filters as Filters
import Api.Profile exposing (Profile)
import Api.User exposing (User)
import BackendTask
import Components.ArticleList
import Components.IconButton as IconButton
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
import Html.Attributes exposing (class, classList, href, src)
import Layout
import MySession
import Pages.PageUrl
import PagesMsg exposing (PagesMsg)
import Path
import RouteBuilder exposing (App)
import Server.Request
import Server.Response
import Shared
import Utils.Form exposing (inProgressText)
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


type alias Data =
    { user : Maybe User
    , profile : Profile
    , listing : Api.Article.Listing
    , selectedTab : Tab
    , page : Int
    }


type Tab
    = MyArticles
    | FavoritedArticles



-- UPDATE


type Msg
    = NoOp


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
view app shared model =
    { title = "Profile"
    , body =
        [ viewProfile app app.data.profile ]
            |> Layout.view app.data.user
    }


viewProfile : App Data ActionData RouteParams -> Profile -> Html (PagesMsg Msg)
viewProfile app profile =
    let
        isViewingOwnProfile : Bool
        isViewingOwnProfile =
            Maybe.map .username app.data.user == Just profile.username

        viewUserInfo : Html (PagesMsg Msg)
        viewUserInfo =
            div [ class "user-info" ]
                [ div [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                            [ img [ class "user-img", src profile.image ] []
                            , h4 [] [ text profile.username ]
                            , Utils.Maybe.view profile.bio
                                (\bio -> p [] [ text bio ])
                            , if isViewingOwnProfile then
                                text ""

                              else
                                Utils.Maybe.view app.data.user <|
                                    \_ ->
                                        followForm
                                            |> Form.toDynamicFetcher
                                            |> Form.renderHtml
                                                ("follow-" ++ app.data.profile.username)
                                                []
                                                (\_ -> Nothing)
                                                app
                                                app.data.profile
                            ]
                        ]
                    ]
                ]

        viewTabRow : Html (PagesMsg Msg)
        viewTabRow =
            div [ class "articles-toggle" ]
                [ ul [ class "nav nav-pills outline-active" ]
                    (List.map viewTab [ MyArticles, FavoritedArticles ])
                ]

        viewTab : Tab -> Html (PagesMsg Msg)
        viewTab tab =
            li [ class "nav-item" ]
                [ a
                    [ class "nav-link"
                    , href
                        (if tab == FavoritedArticles then
                            "?tab=favorites"

                         else
                            "?tab=mine"
                        )
                    , classList [ ( "active", tab == app.data.selectedTab ) ]
                    ]
                    [ text
                        (case tab of
                            MyArticles ->
                                "My Articles"

                            FavoritedArticles ->
                                "Favorited Articles"
                        )
                    ]
                ]
    in
    div [ class "profile-page" ]
        [ viewUserInfo
        , div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-xs-12 col-md-10 offset-md-1" ]
                    (viewTabRow
                        :: Components.ArticleList.view
                            { user = app.data.user
                            , articleListing = app.data.listing
                            , toggleFavoriteView = toggleFavoriteView app
                            , paginationView =
                                filtersForm
                                    |> Form.withGetMethod
                                    |> Form.renderHtml "filters" [] (\_ -> Nothing) app app.data.listing
                            }
                    )
                ]
            ]
        ]


toggleFavoriteView : App Data ActionData RouteParams -> Article -> Html (PagesMsg Msg)
toggleFavoriteView app article =
    favoriteForm
        |> Form.toDynamicFetcher
        |> Form.renderHtml
            (if article.favorited then
                "unfavorite-" ++ article.slug

             else
                "favorite-" ++ article.slug
            )
            []
            (\_ -> Nothing)
            app
            article


type alias RouteParams =
    { username : String }


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
                    ( page, viewFavorites ) =
                        case parsedRequest of
                            ( _, Ok parsedFilters ) ->
                                ( parsedFilters.page |> Maybe.withDefault 1
                                , parsedFilters.page /= Nothing || parsedFilters.globalTab
                                )

                            _ ->
                                ( 1, False )
                in
                BackendTask.map2
                    (\profile listing ->
                        \user ->
                            Server.Response.render
                                { user = user
                                , profile = profile
                                , selectedTab =
                                    if viewFavorites then
                                        FavoritedArticles

                                    else
                                        MyArticles
                                , page = page
                                , listing = listing
                                }
                    )
                    (Api.Profile.get
                        { token = token
                        , username = routeParams.username
                        }
                    )
                    (Api.Article.list
                        { page = page
                        , filters =
                            if viewFavorites then
                                Filters.create |> Filters.favoritedBy routeParams.username

                            else
                                Filters.create |> Filters.byAuthor routeParams.username
                        , token = token
                        }
                    )
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
                            Ok (Favorite { slug, setFavorite }) ->
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

                            Ok (Follow { setFollowing }) ->
                                case token of
                                    Just justToken ->
                                        (if setFollowing then
                                            Api.Profile.follow

                                         else
                                            Api.Profile.unfollow
                                        )
                                            { token = justToken
                                            , username = routeParams.username
                                            }
                                            |> BackendTask.map
                                                (\_ _ -> Server.Response.render {})

                                    Nothing ->
                                        BackendTask.succeed (\_ -> Server.Response.render {})

                            Err _ ->
                                BackendTask.succeed (\_ -> Server.Response.render {})
            )


type alias FavoriteAction =
    { slug : String
    , setFavorite : Bool
    }


type Action
    = Favorite FavoriteAction
    | Follow FollowAction


favoriteForm : Form.HtmlForm String FavoriteAction Article Msg
favoriteForm =
    (\slug setFavorite ->
        { combine =
            Form.Validation.succeed FavoriteAction
                |> Form.Validation.andMap slug
                |> Form.Validation.andMap setFavorite
        , view =
            \formState ->
                let
                    article : Article
                    article =
                        formState.data
                in
                [ if formState.data.favorited then
                    IconButton.view
                        { color = IconButton.FilledGreen
                        , icon = IconButton.Heart
                        , label = inProgressText formState <| " " ++ String.fromInt article.favoritesCount
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGreen
                        , icon = IconButton.Heart
                        , label = inProgressText formState <| " " ++ String.fromInt article.favoritesCount
                        }
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "slug" (Form.Field.required "Required" Form.Field.text |> Form.Field.withInitialValue (.slug >> Form.Value.string))
        |> Form.hiddenField "set-favorite" (Form.Field.checkbox |> Form.Field.withInitialValue (.favorited >> not >> Form.Value.bool))
        |> Form.hiddenKind ( "kind", "favorite" ) "Expected kind."


followForm : Form.HtmlForm String FollowAction Profile Msg
followForm =
    (\setFollow ->
        { combine =
            Form.Validation.succeed FollowAction
                |> Form.Validation.andMap setFollow
        , view =
            \formState ->
                [ if formState.data.following then
                    IconButton.view
                        { color = IconButton.FilledGray
                        , icon = IconButton.Plus
                        , label = inProgressText formState <| "Unfollow " ++ formState.data.username
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGray
                        , icon = IconButton.Plus
                        , label = inProgressText formState <| "Follow " ++ formState.data.username
                        }
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "set-follow" (Form.Field.checkbox |> Form.Field.withInitialValue (.following >> not >> Form.Value.bool))
        |> Form.hiddenKind ( "kind", "follow" ) "Expected kind."


type alias FollowAction =
    { setFollowing : Bool
    }


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Favorite favoriteForm
        |> Form.combine Follow followForm


type alias ParsedFilters =
    { globalTab : Bool
    , page : Maybe Int
    }


filtersForm : Form.HtmlForm String ParsedFilters Api.Article.Listing Msg
filtersForm =
    (\tab pageNumber ->
        { combine =
            Form.Validation.succeed
                (\tabString number ->
                    ParsedFilters
                        (tabString == Just "favorites")
                        number
                )
                |> Form.Validation.andMap tab
                |> Form.Validation.andMap pageNumber
        , view =
            \formState ->
                let
                    viewPage : Int -> Html msg
                    viewPage page =
                        li
                            [ class "page-item"
                            , classList [ ( "active", formState.data.page == page ) ]
                            ]
                            [ pageNumber
                                |> Form.FieldView.valueButton (String.fromInt page)
                                    [ class "page-link"
                                    ]
                                    [ text (String.fromInt page) ]
                            ]
                in
                [ List.range 1 formState.data.totalPages
                    |> List.map viewPage
                    |> ul [ class "pagination" ]
                ]
        }
    )
        |> Form.init
        |> Form.field "tab" Form.Field.text
        |> Form.field "page"
            (Form.Field.int { invalid = \_ -> "Expected int." }
                |> Form.Field.withInitialValue (.page >> Form.Value.int)
            )
