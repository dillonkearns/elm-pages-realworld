module Route.Article.Slug_ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Api.Article exposing (Article)
import Api.Article.Comment exposing (Comment)
import Api.Profile exposing (Profile)
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
import Html.Attributes exposing (attribute, class, placeholder, src, style)
import Layout
import Markdown
import MySession
import Pages.Msg
import Pages.PageUrl
import Path
import Platform.Sub
import RouteBuilder
import Server.Request
import Server.Response
import Shared
import Utils.Maybe
import Utils.Time
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
view maybeUrl shared model app =
    { title = app.data.article.title
    , body =
        [ viewArticle app app.data.article
        ]
            |> Layout.view app.data.user
    }


viewArticle : RouteBuilder.StaticPayload Data ActionData RouteParams -> Article -> Html (Pages.Msg.Msg Msg)
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


viewArticleMeta : RouteBuilder.StaticPayload Data ActionData RouteParams -> Article -> Html (Pages.Msg.Msg Msg)
viewArticleMeta app article =
    div [ class "article-meta" ] <|
        List.concat
            [ [ a
                    [--href ("/profile/" ++ article.author.username)
                    ]
                    [ img [ src article.author.image ] []
                    ]
              , div [ class "info" ]
                    [ a
                        [ class "author"

                        --, href ("/profile/" ++ article.author.username)
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


viewControls : RouteBuilder.StaticPayload Data ActionData RouteParams -> Article -> User -> List (Html (Pages.Msg.Msg Msg))
viewControls app article user =
    if article.author.username == user.username then
        [ a
            [ class "btn btn-outline-secondary btn-sm"

            --, href ("/editor/" ++ article.slug)
            ]
            [ i [ class "ion-edit" ] []
            , text "Edit article"
            ]
        ]

    else
        [ Form.renderHtml
            [ style "display" "inline"
            ]
            (\_ -> Nothing)
            app
            article
            (Form.toDynamicTransition "follow" followForm)
        , Form.renderHtml
            [ style "display" "inline"
            ]
            (\_ -> Nothing)
            app
            article
            (Form.toDynamicTransition "favorite" favoriteForm)
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

                    ellipsesIfInProgress : String
                    ellipsesIfInProgress =
                        if formState.isTransitioning then
                            "..."

                        else
                            ""
                in
                [ if formState.data.favorited then
                    IconButton.view
                        { color = IconButton.FilledGreen
                        , icon = IconButton.Heart
                        , label =
                            --" " ++ String.fromInt article.favoritesCount ++ ellipsesIfInProgress
                            "Unfavorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGreen
                        , icon = IconButton.Heart
                        , label =
                            --" " ++ String.fromInt article.favoritesCount ++ ellipsesIfInProgress
                            "Favorite Post (" ++ String.fromInt article.favoritesCount ++ ")"
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
                let
                    author : Profile
                    author =
                        formState.data.author

                    ellipsesIfInProgress : String
                    ellipsesIfInProgress =
                        if formState.isTransitioning then
                            "..."

                        else
                            ""
                in
                [ if author.following then
                    IconButton.view
                        { color = IconButton.FilledGray
                        , icon = IconButton.Plus
                        , label = "Unfollow " ++ author.username ++ ellipsesIfInProgress
                        }

                  else
                    IconButton.view
                        { color = IconButton.OutlinedGray
                        , icon = IconButton.Plus
                        , label = "Follow " ++ author.username ++ ellipsesIfInProgress
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
                    , style "border" "none"
                    ]
                    [ i [ class "ion-trash-a" ] [] ]
                ]
        }
    )
        |> Form.init
        |> Form.hiddenField "id" (Form.Field.required "Required" (Form.Field.int { invalid = \_ -> "Invalid ID, must be int." }) |> Form.Field.withInitialValue Form.Value.int)
        |> Form.hiddenKind ( "kind", "delete-comment" ) "Expected kind."


commentForm : Form.HtmlForm String String User Msg
commentForm =
    (\comment ->
        { combine =
            Form.Validation.succeed identity
                |> Form.Validation.andMap comment
        , view =
            \formState ->
                let
                    ellipsesIfInProgress : String
                    ellipsesIfInProgress =
                        if formState.isTransitioning then
                            "..."

                        else
                            ""
                in
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
                        [ text ("Post Comment" ++ ellipsesIfInProgress) ]
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


formHandlers : Form.ServerForms String Action
formHandlers =
    Form.initCombined Favorite favoriteForm
        |> Form.combine Follow followForm
        |> Form.combine SubmitComment commentForm
        |> Form.combine DeleteComment deleteCommentForm


viewCommentSection : RouteBuilder.StaticPayload Data ActionData RouteParams -> Article -> Html (Pages.Msg.Msg Msg)
viewCommentSection app article =
    div [ class "row" ]
        [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
            List.concat
                [ case app.data.user of
                    Just user ->
                        [ Form.renderHtml
                            [ class "card comment-form" ]
                            (\_ -> Nothing)
                            app
                            user
                            (Form.toDynamicTransition "submit-comment" commentForm)
                        ]

                    Nothing ->
                        []
                , List.map (viewComment app app.data.user article) app.data.comments
                ]
        ]



--viewComment : Maybe User -> Article -> Comment -> Html (Pages.Msg.Msg Msg)


viewComment app currentUser article comment =
    let
        viewCommentActions =
            Utils.Maybe.view currentUser <|
                \user ->
                    if user.username == comment.author.username then
                        Form.renderHtml
                            [ style "display" "inline"
                            ]
                            (\_ -> Nothing)
                            app
                            comment.id
                            (Form.toDynamicTransition ("delete-comment-" ++ String.fromInt comment.id) deleteCommentForm)

                    else
                        text ""
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text comment.body ] ]
        , div [ class "card-footer" ]
            [ a
                [ class "comment-author"

                --, href ("/profile/" ++ comment.author.username)
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
                        case parsedForm |> Debug.log "formAction" of
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
                                                (\_ ->
                                                    \_ ->
                                                        Server.Response.render {}
                                                )

                                    Nothing ->
                                        BackendTask.succeed (\_ -> Server.Response.render {})

                            Ok (Follow { username, setFollowing }) ->
                                case token of
                                    Just justToken ->
                                        (if setFollowing then
                                            Api.Profile.follow

                                         else
                                            Api.Profile.unfollow
                                        )
                                            { token = justToken
                                            , username = username
                                            }
                                            |> BackendTask.map
                                                (\_ ->
                                                    \_ ->
                                                        Server.Response.render {}
                                                )

                                    Nothing ->
                                        BackendTask.succeed (\_ -> Server.Response.render {})

                            Ok (SubmitComment comment) ->
                                case token of
                                    Just justToken ->
                                        Api.Article.Comment.create
                                            { token = justToken
                                            , articleSlug = routeParams.slug
                                            , comment = { body = comment }
                                            }
                                            |> BackendTask.map
                                                (\_ ->
                                                    \_ ->
                                                        Server.Response.render {}
                                                )

                                    Nothing ->
                                        BackendTask.succeed (\_ -> Server.Response.render {})

                            Ok (DeleteComment commentId) ->
                                case token of
                                    Just justToken ->
                                        Api.Article.Comment.delete
                                            { token = justToken
                                            , articleSlug = routeParams.slug
                                            , commentId = commentId
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



--type Msg
--    | ClickedDeleteArticle User Article
--    | DeletedArticle (Data Article)
--
--
--update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
--update req msg model =
--    case msg of
--        ClickedDeleteArticle user article ->
--            ( model
--            , Api.Article.delete
--                { token = user.token
--                , slug = article.slug
--                , onResponse = DeletedArticle
--                }
--            )
--
--        DeletedArticle _ ->
--            ( model
--            , Utils.Route.navigate req.key Route.Home_
--            )
