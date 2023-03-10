module Api.Article exposing
    ( Article, decoder
    , Listing
    , list, feed
    , get, create, update, delete
    , favorite, unfavorite
    )

{-|

@docs Article, decoder
@docs Listing
@docs list, feed
@docs get, create, update, delete
@docs favorite, unfavorite

-}

import Api.Article.Filters as Filters exposing (Filters)
import Api.Profile exposing (Profile)
import Api.Token exposing (Token)
import BackendTask exposing (BackendTask)
import BackendTask.Http
import FatalError exposing (FatalError)
import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time
import Utils.Json exposing (withField)


type alias Article =
    { slug : String
    , title : String
    , description : String
    , body : String
    , tags : List String
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , favorited : Bool
    , favoritesCount : Int
    , author : Profile
    }


decoder : Json.Decoder Article
decoder =
    Utils.Json.record Article
        |> withField "slug" Json.string
        |> withField "title" Json.string
        |> withField "description" Json.string
        |> withField "body" Json.string
        |> withField "tagList" (Json.list Json.string)
        |> withField "createdAt" Iso8601.decoder
        |> withField "updatedAt" Iso8601.decoder
        |> withField "favorited" Json.bool
        |> withField "favoritesCount" Json.int
        |> withField "author" Api.Profile.decoder


type alias Listing =
    { articles : List Article
    , page : Int
    , totalPages : Int
    }



-- ENDPOINTS


itemsPerPage : Int
itemsPerPage =
    25


list : { a | token : Maybe Token, page : Int, filters : Filters } -> BackendTask FatalError Listing
list options =
    Api.Token.get options.token
        { url = "https://api.realworld.io/api/articles/" ++ Filters.toQueryString options.page options.filters
        , expect = paginatedDecoder options.page |> BackendTask.Http.expectJson
        }


feed :
    { token : Token
    , page : Int
    }
    -> BackendTask FatalError Listing
feed options =
    Api.Token.get (Just options.token)
        { url = "https://api.realworld.io/api/articles/feed" ++ Filters.pageQueryParameters options.page
        , expect = paginatedDecoder options.page |> BackendTask.Http.expectJson
        }


get :
    { slug : String
    , token : Maybe Token
    }
    -> BackendTask FatalError Article
get options =
    Api.Token.get options.token
        { url = "https://api.realworld.io/api/articles/" ++ options.slug
        , expect = Json.field "article" decoder |> BackendTask.Http.expectJson
        }


create :
    { token : Token
    , article :
        { article
            | title : String
            , description : String
            , body : String
            , tags : List String
        }
    }
    -> BackendTask FatalError (Result (List String) Article)
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "article"
                  , Encode.object
                        [ ( "title", Encode.string options.article.title )
                        , ( "description", Encode.string options.article.description )
                        , ( "body", Encode.string options.article.body )
                        , ( "tagList", Encode.list Encode.string options.article.tags )
                        ]
                  )
                ]
    in
    Api.Token.requestWithErrors "POST"
        (BackendTask.Http.jsonBody body)
        (Just options.token)
        { url = "https://api.realworld.io/api/articles"
        , expect =
            Json.field "article" decoder
        }


update :
    { slug : String }
    ->
        { token : Token
        , article :
            { article
                | title : String
                , description : String
                , body : String
            }
        }
    -> BackendTask FatalError (Result (List String) Article)
update { slug } options =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "article"
                  , Encode.object
                        [ ( "title", Encode.string options.article.title )
                        , ( "description", Encode.string options.article.description )
                        , ( "body", Encode.string options.article.body )
                        ]
                  )
                ]
    in
    Api.Token.requestWithErrors "PUT"
        (BackendTask.Http.jsonBody body)
        (Just options.token)
        { url = "https://api.realworld.io/api/articles/" ++ slug
        , expect = Json.field "article" decoder
        }


delete :
    { token : Token
    , slug : String
    }
    -> BackendTask FatalError ()
delete options =
    Api.Token.delete
        (Just options.token)
        { url = "https://api.realworld.io/api/articles/" ++ options.slug
        , expect = BackendTask.Http.expectWhatever ()
        }


favorite : { token : Token, slug : String } -> BackendTask FatalError Article
favorite options =
    Api.Token.post (Just options.token)
        { url = "https://api.realworld.io/api/articles/" ++ options.slug ++ "/favorite"
        , body = BackendTask.Http.emptyBody
        , expect = Json.field "article" decoder |> BackendTask.Http.expectJson
        }


unfavorite : { token : Token, slug : String } -> BackendTask FatalError Article
unfavorite options =
    Api.Token.delete (Just options.token)
        { url = "https://api.realworld.io/api/articles/" ++ options.slug ++ "/favorite"
        , expect = Json.field "article" decoder |> BackendTask.Http.expectJson
        }



-- INTERNALS


paginatedDecoder : Int -> Json.Decoder Listing
paginatedDecoder page =
    let
        multipleArticles : List Article -> Int -> Listing
        multipleArticles articles count =
            { articles = articles
            , page = page
            , totalPages = count // itemsPerPage
            }
    in
    Json.map2 multipleArticles
        (Json.field "articles" (Json.list decoder))
        (Json.field "articlesCount" Json.int)
