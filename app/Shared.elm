module Shared exposing (Data, Model, Msg(..), SharedMsg(..), template)

import BackendTask exposing (BackendTask)
import Components.Footer
import Components.Navbar
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Nothing
    }


type Msg
    = SharedMsg SharedMsg
    | MenuClicked


type alias Data =
    ()


type SharedMsg
    = NoOp


type alias Model =
    { showMenu : Bool
    }


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init flags maybePagePath =
    ( { showMenu = False }
    , Effect.none
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SharedMsg globalMsg ->
            ( model, Effect.none )

        MenuClicked ->
            ( { model | showMenu = not model.showMenu }, Effect.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : BackendTask FatalError Data
data =
    BackendTask.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : List (Html msg), title : String }
view sharedData page model toMsg pageView =
    { title =
        if String.isEmpty pageView.title then
            "Conduit"

        else
            pageView.title ++ " | Conduit"
    , body =
        [ div [ class "layout" ]
            [ Components.Navbar.view
                { user = Nothing --model.user
                , currentRoute = page.route |> Maybe.withDefault Route.Index

                --Utils.Route.fromUrl req.url
                , onSignOut = toMsg MenuClicked --toMsg ClickedSignOut
                }
            , div [ class "page" ] pageView.body
            , Components.Footer.view
            ]
        ]
    }
