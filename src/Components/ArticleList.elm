module Components.ArticleList exposing (view)

import Api.Article exposing (Article)
import Api.User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (alt, class, src)
import Route
import Utils.Maybe
import Utils.Time


view :
    { user : Maybe User
    , articleListing : Api.Article.Listing
    , toggleFavoriteView : Article -> Html msg
    , paginationView : Html msg
    }
    -> List (Html msg)
view options =
    List.concat
        [ List.map (viewArticlePreview options) options.articleListing.articles
        , [ options.paginationView
          ]
        ]


viewArticlePreview :
    { options
        | user : Maybe User
        , toggleFavoriteView : Article -> Html msg
    }
    -> Article
    -> Html msg
viewArticlePreview options article =
    div [ class "article-preview" ]
        [ div [ class "article-meta" ]
            [ Route.Profile__Username_ { username = article.author.username }
                |> Route.link
                    []
                    [ img [ src article.author.image, alt article.author.username ] []
                    ]
            , div [ class "info" ]
                [ Route.Profile__Username_ { username = article.author.username }
                    |> Route.link
                        [ class "author"
                        ]
                        [ text article.author.username ]
                , span [ class "date" ] [ text (Utils.Time.formatDate article.createdAt) ]
                ]
            , div [ class "pull-xs-right" ]
                [ Utils.Maybe.view options.user <|
                    \user ->
                        if user.username == article.author.username then
                            text ""

                        else
                            options.toggleFavoriteView article
                ]
            ]
        , Route.Article__Slug_ { slug = article.slug }
            |> Route.link
                [ class "preview-link"
                ]
                [ h1 [] [ text article.title ]
                , p [] [ text article.description ]
                , span [] [ text "Read more..." ]
                , if List.isEmpty article.tags then
                    text ""

                  else
                    ul [ class "tag-list" ]
                        (List.map
                            (\tag -> li [ class "tag-default tag-pill tag-outline" ] [ text tag ])
                            article.tags
                        )
                ]
        ]
