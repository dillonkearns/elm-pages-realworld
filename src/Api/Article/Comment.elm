module Api.Article.Comment exposing
    ( Comment
    , decoder
    , get, create, delete
    )

{-|

@docs Comment
@docs decoder
@docs get, create, delete

-}

import Api.Profile exposing (Profile)
import Api.Token exposing (Token)
import BackendTask exposing (BackendTask)
import BackendTask.Http
import FatalError exposing (FatalError)
import Iso8601
import Json.Decode as Json
import Json.Encode as Encode
import Time


type alias Comment =
    { id : Int
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    , body : String
    , author : Profile
    }


decoder : Json.Decoder Comment
decoder =
    Json.map5 Comment
        (Json.field "id" Json.int)
        (Json.field "createdAt" Iso8601.decoder)
        (Json.field "updatedAt" Iso8601.decoder)
        (Json.field "body" Json.string)
        (Json.field "author" Api.Profile.decoder)



-- ENDPOINTS


get :
    { token : Maybe Token
    , articleSlug : String
    }
    -> BackendTask FatalError (List Comment)
get options =
    Api.Token.get options.token
        { url = "https://api.realworld.io/api/articles/" ++ options.articleSlug ++ "/comments"
        , expect = Json.field "comments" (Json.list decoder)
        }


create :
    { token : Token
    , articleSlug : String
    , comment : { comment | body : String }
    }
    -> BackendTask FatalError Comment
create options =
    let
        body : Json.Value
        body =
            Encode.object
                [ ( "comment"
                  , Encode.object
                        [ ( "body", Encode.string options.comment.body )
                        ]
                  )
                ]
    in
    Api.Token.post (Just options.token)
        { url = "https://api.realworld.io/api/articles/" ++ options.articleSlug ++ "/comments"
        , body = BackendTask.Http.jsonBody body
        , expect =
            Json.field "comment" decoder
        }


delete :
    { token : Token
    , articleSlug : String
    , commentId : Int
    }
    -> BackendTask FatalError Int
delete options =
    Api.Token.delete (Just options.token)
        { url =
            "https://api.realworld.io/api/articles/"
                ++ options.articleSlug
                ++ "/comments/"
                ++ String.fromInt options.commentId
        , expect =
            Json.succeed options.commentId
        }
