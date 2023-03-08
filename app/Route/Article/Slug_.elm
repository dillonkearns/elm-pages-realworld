module Route.Article.Slug_ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import Api.Article.Comment exposing (Comment)
import Api.Profile
import Api.User exposing (User)
import BackendTask exposing (BackendTask)
import Components.IconButton as IconButton
import Effect
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Form
import Form.Field
import Form.FieldView
import Form.Validation
import Form.Value
import Head
import Html exposing (..)
import Html.Attributes exposing (class, placeholder, src, style)
import Layout
import Markdown
import MySession
import PagesMsg exposing (PagesMsg)
import Path
import Route
import RouteBuilder exposing (App)
import Server.Request
import Server.Response
import Shared
import Utils.Form exposing (inProgressText)
import Utils.Maybe
import Utils.Time
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
view app shared model =
    { title = app.data.article.title
    , body =
        [ viewArticle app app.data.article
        ]
            |> Layout.view app app.data.user
    }


viewArticle : App Data ActionData RouteParams -> Article -> Html (PagesMsg Msg)
viewArticle app article =
    div [ class "article-page" ]
        [ div [ class "banner" ]
            [ div [ class "container" ]
                [ h1 [] [ text article.title ]
                , viewArticleMeta app article
                ]
            ]
        , div [ class "container page" ]
            [ div [ class "row article-content" ]
                [ div [ class "col-md-12" ]
                    [ Markdown.toHtml [] article.body ]
                , if List.isEmpty article.tags then
                    text ""

                  else
                    ul [ class "tag-list" ]
                        (List.map
                            (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                            article.tags
                        )
                ]
            , hr [] []
            , div [ class "article-actions" ] [ viewArticleMeta app article ]
            , viewCommentSection app article
            ]
        ]


viewArticleMeta : App Data ActionData RouteParams -> Article -> Html (PagesMsg Msg)
viewArticleMeta app article =
    div [ class "article-meta" ] <|
        List.concat
            [ [ Route.Profile__Username_ { username = article.author.username }
                    |> Route.link []
                        [ img [ src article.author.image ] []
                        ]
              , div [ class "info" ]
                    [ Route.Profile__Username_ { username = article.author.username }
                        |> Route.link
                            [ class "author"
                            ]
                            [ text article.author.username ]
                    , span [ class "date" ] [ text (Utils.Time.formatDate article.createdAt) ]
                    ]
              ]
            , case app.data.user of
                Just user ->
                    viewControls app article user

                Nothing ->
                    []
            ]


viewControls : App Data ActionData RouteParams -> Article -> User -> List (Html (PagesMsg Msg))
viewControls app article user =
    if article.author.username == user.username then
        [ Route.Editor__Slug__ { slug = Just article.slug }
            |> Route.link
                [ class "btn btn-outline-secondary btn-sm"
                ]
                [ i [ class "ion-edit" ] []
                , text "Edit article"
                ]
        , Form.renderHtml "delete-article"
            [ style "display" "inline"
            ]
            (\_ -> Nothing)
            app
            ()
            deleteArticleForm
        ]

    else
        [ Form.renderHtml "follow"
            [ style "display" "inline"
            ]
            (\_ -> Nothing)
            app
            article
            followForm
        , Form.renderHtml "favorite"
            [ style "display" "inline"
            ]
            (\_ -> Nothing)
            app
            article
            favoriteForm
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
                let
                    article : Article
                    article =
                        formState.data
                in
                [ if formState.data.favorited then
                    IconButton.view
                        { color = IconButton.FilledGreen
                        , icon = IconButton.Heart
                        , label = "Unfavorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGreen
                        , icon = IconButton.Heart
                        , label = "Favorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                        }
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "slug" (Form.Field.required "Required" Form.Field.text |> Form.Field.withInitialValue (.slug >> Form.Value.string))
        |> Form.hiddenField "set-favorite" (Form.Field.checkbox |> Form.Field.withInitialValue (.favorited >> not >> Form.Value.bool))
        |> Form.hiddenKind ( "kind", "favorite" ) "Expected kind."


followForm : Form.HtmlForm String FollowAction Article Msg
followForm =
    (\username setFollow ->
        { combine =
            Form.Validation.succeed FollowAction
                |> Form.Validation.andMap username
                |> Form.Validation.andMap setFollow
        , view =
            \formState ->
                [ if formState.data.author.following then
                    IconButton.view
                        { color = IconButton.FilledGray
                        , icon = IconButton.Plus
                        , label = inProgressText formState <| "Unfollow " ++ formState.data.author.username
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGray
                        , icon = IconButton.Plus
                        , label = inProgressText formState <| "Follow " ++ formState.data.author.username
                        }
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "username" (Form.Field.required "Required" Form.Field.text |> Form.Field.withInitialValue (.author >> .username >> Form.Value.string))
        |> Form.hiddenField "set-follow" (Form.Field.checkbox |> Form.Field.withInitialValue (.author >> .following >> not >> Form.Value.bool))
        |> Form.hiddenKind ( "kind", "follow" ) "Expected kind."


deleteCommentForm : Form.HtmlForm String Int Int Msg
deleteCommentForm =
    (\id ->
        { combine =
            Form.Validation.succeed identity
                |> Form.Validation.andMap id
        , view =
            \formState ->
                [ button
                    [ class "mod-options"
                    , Html.Attributes.disabled formState.isTransitioning
                    ]
                    [ i [ class "ion-trash-a" ] [] ]
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "id" (Form.Field.required "Required" (Form.Field.int { invalid = \_ -> "Invalid ID, must be int." }) |> Form.Field.withInitialValue Form.Value.int)
        |> Form.hiddenKind ( "kind", "delete-comment" ) "Expected kind."


deleteArticleForm : Form.HtmlForm String () input Msg
deleteArticleForm =
    { combine =
        Form.Validation.succeed ()
    , view =
        \formState ->
            [ IconButton.view
                { color = IconButton.OutlinedRed
                , icon = IconButton.Trash
                , label = inProgressText formState "Delete article"
                }
            ]
    }
        |> Form.init
        |> Form.hiddenKind ( "kind", "delete-article" ) "Expected kind."


commentForm : Form.HtmlForm String String User Msg
commentForm =
    (\comment ->
        { combine =
            Form.Validation.succeed identity
                |> Form.Validation.andMap comment
        , view =
            \formState ->
                [ div [ class "card-block" ]
                    [ Form.FieldView.input
                        [ class "form-control"
                        , placeholder "Write a comment..."
                        ]
                        comment
                    ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", src formState.data.image ] []
                    , button [ class "btn btn-sm btn-primary", Html.Attributes.disabled formState.isTransitioning ]
                        [ text (inProgressText formState "Post Comment") ]
                    ]
                ]
        }
    )
        |> Form.init
        |> Form.hiddenKind ( "kind", "create-comment" ) "Expected kind."
        |> Form.field "comment"
            (Form.Field.textarea
                { rows = Just 3, cols = Nothing }
                (Form.Field.required "Required" Form.Field.text)
            )


type alias FavoriteAction =
    { slug : String
    , setFavorite : Bool
    }


type alias FollowAction =
    { username : String
    , setFollowing : Bool
    }


type Action
    = Favorite FavoriteAction
    | Follow FollowAction
    | SubmitComment String
    | DeleteComment Int
    | DeleteArticle


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Favorite favoriteForm
        |> Form.combine Follow followForm
        |> Form.combine SubmitComment commentForm
        |> Form.combine DeleteComment deleteCommentForm
        |> Form.combine (\() -> DeleteArticle) deleteArticleForm


viewCommentSection : App Data ActionData RouteParams -> Article -> Html (PagesMsg Msg)
viewCommentSection app article =
    div [ class "row" ]
        [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
            List.concat
                [ case app.data.user of
                    Just user ->
                        [ Form.renderHtml "submit-comment"
                            [ class "card comment-form" ]
                            (\_ -> Nothing)
                            app
                            user
                            commentForm
                        ]

                    Nothing ->
                        []
                , List.map (viewComment app app.data.user) app.data.comments
                ]
        ]


viewComment : App Data ActionData RouteParams -> Maybe User -> Comment -> Html (PagesMsg Msg)
viewComment app currentUser comment =
    let
        viewCommentActions =
            Utils.Maybe.view currentUser <|
                \user ->
                    if user.username == comment.author.username then
                        Form.renderHtml ("delete-comment-" ++ String.fromInt comment.id)
                            [ style "display" "inline"
                            ]
                            (\_ -> Nothing)
                            app
                            comment.id
                            deleteCommentForm

                    else
                        text ""
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text comment.body ] ]
        , div [ class "card-footer" ]
            [ Route.Profile__Username_ { username = comment.author.username }
                |> Route.link
                    [ class "comment-author"
                    ]
                    [ img [ class "comment-author-img", src comment.author.image ] []
                    , text comment.author.username
                    ]
            , span [ class "date-posted" ] [ text (Utils.Time.formatDate comment.createdAt) ]
            , viewCommentActions
            ]
        ]


type alias RouteParams =
    { slug : String }


type alias Data =
    { article : Article
    , comments : List Comment
    , user : Maybe User
    }


type alias ActionData =
    {}


data :
    RouteParams
    -> Server.Request.Parser (BackendTask FatalError (Server.Response.Response Data ErrorPage))
data routeParams =
    Server.Request.succeed
        ()
        |> MySession.withUser
            (\{ token } ->
                BackendTask.map2
                    (\article comments ->
                        \user ->
                            Server.Response.render
                                { article = article
                                , comments = comments
                                , user = user
                                }
                    )
                    (Api.Article.get
                        { slug = routeParams.slug
                        , token = token
                        }
                    )
                    (Api.Article.Comment.get
                        { token = token
                        , articleSlug = routeParams.slug
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
                case ( token, parsedRequest ) of
                    ( Nothing, _ ) ->
                        BackendTask.succeed (\_ -> Server.Response.render {})

                    ( Just justToken, ( _, parsedForm ) ) ->
                        case parsedForm of
                            Ok (Favorite { slug, setFavorite }) ->
                                (if setFavorite then
                                    Api.Article.favorite

                                 else
                                    Api.Article.unfavorite
                                )
                                    { token = justToken, slug = slug }
                                    |> BackendTask.map
                                        (\_ _ -> Server.Response.render {})

                            Ok (Follow { username, setFollowing }) ->
                                (if setFollowing then
                                    Api.Profile.follow

                                 else
                                    Api.Profile.unfollow
                                )
                                    { token = justToken, username = username }
                                    |> BackendTask.map
                                        (\_ _ -> Server.Response.render {})

                            Ok (SubmitComment comment) ->
                                Api.Article.Comment.create
                                    { token = justToken
                                    , articleSlug = routeParams.slug
                                    , comment = { body = comment }
                                    }
                                    |> BackendTask.map
                                        (\_ _ -> Server.Response.render {})

                            Ok (DeleteComment commentId) ->
                                Api.Article.Comment.delete
                                    { token = justToken
                                    , articleSlug = routeParams.slug
                                    , commentId = commentId
                                    }
                                    |> BackendTask.map
                                        (\_ _ -> Server.Response.render {})

                            Ok DeleteArticle ->
                                Api.Article.delete
                                    { token = justToken
                                    , slug = routeParams.slug
                                    }
                                    |> BackendTask.map
                                        (\_ _ -> Route.redirectTo Route.Index)

                            Err _ ->
                                BackendTask.succeed (\_ -> Server.Response.render {})
            )
