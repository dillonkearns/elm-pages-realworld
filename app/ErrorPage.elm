module ErrorPage exposing (ErrorPage(..), Model, Msg, head, init, internalError, notFound, statusCode, update, view)

import Components.NotFound
import Effect exposing (Effect)
import Head
import View exposing (View)


type Msg
    = NoOp


type alias Model =
    {}


init : ErrorPage -> ( Model, Effect Msg )
init errorPage =
    ( {}
    , Effect.none
    )


update : ErrorPage -> Msg -> Model -> ( Model, Effect Msg )
update errorPage msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


head : ErrorPage -> List Head.Tag
head errorPage =
    []


type ErrorPage
    = NotFound
    | InternalError String


notFound : ErrorPage
notFound =
    NotFound


internalError : String -> ErrorPage
internalError =
    InternalError


view : ErrorPage -> Model -> View Msg
view error model =
    { title = "404"
    , body =
        [ Components.NotFound.view
        ]
    }


statusCode : ErrorPage -> number
statusCode error =
    case error of
        NotFound ->
            404

        InternalError _ ->
            500
