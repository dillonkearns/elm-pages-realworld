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
import Form.Validation
import Form.Value
import Head
import Html exposing (..)
import Html.Attributes exposing (class, src)
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



--fetchArticlesBy : Maybe Token -> String -> Int -> Cmd Msg
--fetchArticlesBy token username page_ =
--    Api.Article.list
--        { token = token
--        , page = page_
--        , filters = Filters.create |> Filters.byAuthor username
--        , onResponse = GotArticles
--        }
--
--
--fetchArticlesFavoritedBy : Maybe Token -> String -> Int -> Cmd Msg
--fetchArticlesFavoritedBy token username page_ =
--    Api.Article.list
--        { token = token
--        , page = page_
--        , filters =
--            Filters.create |> Filters.favoritedBy username
--        , onResponse = GotArticles
--        }
-- UPDATE


type Msg
    = NoOp



--type Msg
--    | Clicked Tab
--    | ClickedPage Int
--
--
--update : Shared.Model -> Msg -> Model -> ( Model, Cmd Msg )
--update shared msg model =
--    case msg of
--        Clicked MyArticles ->
--            ( { model
--                | selectedTab = MyArticles
--                , listing = Api.Data.Loading
--                , page = 1
--              }
--            , fetchArticlesBy (Maybe.map .token shared.user) model.username 1
--            )
--
--        Clicked FavoritedArticles ->
--            ( { model
--                | selectedTab = FavoritedArticles
--                , listing = Api.Data.Loading
--                , page = 1
--              }
--            , fetchArticlesFavoritedBy (Maybe.map .token shared.user) model.username 1
--            )
--
--        ClickedPage page_ ->
--            let
--                fetch : Maybe Token -> String -> Int -> Cmd Msg
--                fetch =
--                    case model.selectedTab of
--                        MyArticles ->
--                            fetchArticlesBy
--
--                        FavoritedArticles ->
--                            fetchArticlesFavoritedBy
--            in
--            ( { model
--                | listing = Api.Data.Loading
--                , page = page_
--              }
--            , fetch
--                (shared.user |> Maybe.map .token)
--                model.username
--                page_
--            )


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
    { title = "Profile"
    , body =
        [ viewProfile app app.data.profile ]
            |> Layout.view app.data.user
    }


viewProfile : RouteBuilder.StaticPayload Data ActionData RouteParams -> Profile -> Html (Pages.Msg.Msg Msg)
viewProfile app profile =
    let
        isViewingOwnProfile : Bool
        isViewingOwnProfile =
            Maybe.map .username app.data.user == Just profile.username

        viewUserInfo : Html (Pages.Msg.Msg Msg)
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
                                        Form.renderHtml []
                                            (\_ -> Nothing)
                                            app
                                            app.data.profile
                                            (Form.toDynamicFetcher ("follow-" ++ app.data.profile.username) followForm)
                            ]
                        ]
                    ]
                ]

        viewTabRow : Html (Pages.Msg.Msg Msg)
        viewTabRow =
            div [ class "articles-toggle" ]
                [ ul [ class "nav nav-pills outline-active" ]
                    (List.map viewTab [ MyArticles, FavoritedArticles ])
                ]

        viewTab : Tab -> Html (Pages.Msg.Msg Msg)
        viewTab tab =
            li [ class "nav-item" ]
                [ button
                    [ class "nav-link"

                    --, Events.onClick (Clicked tab)
                    --, classList [ ( "active", tab == model.selectedTab ) ]
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
                                Html.text "TODO"

                            --filtersForm
                            --    |> Form.toDynamicTransition "filters"
                            --    |> Form.withGetMethod
                            --    |> Form.renderHtml [] (\_ -> Nothing) app ( RenderPages, app.data.listing )
                            }
                    )
                ]
            ]
        ]


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


type alias RouteParams =
    { username : String }


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
                    (\profile listing ->
                        \user ->
                            Server.Response.render
                                { user = user
                                , profile = profile
                                , selectedTab =
                                    -- TODO check query params for active tab
                                    MyArticles
                                , page =
                                    -- TODO get page from query params
                                    1
                                , listing = listing
                                }
                    )
                    (Api.Profile.get
                        { token = token
                        , username = routeParams.username
                        }
                    )
                    (Api.Article.list
                        { page =
                            -- TODO get page from query params
                            1
                        , filters = Filters.create |> Filters.byAuthor routeParams.username
                        , token = token
                        }
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
