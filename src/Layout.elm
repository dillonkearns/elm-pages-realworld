module Layout exposing (view)

import Components.Footer
import Components.Navbar
import Html exposing (..)
import Html.Attributes exposing (class)
import Route


view app user inner =
    [ div [ class "layout" ]
        [ Components.Navbar.view
            { user = user
            , currentRoute = Route.Index
            }
        , div
            [ case app.transition of
                Just _ ->
                    class "page loading"

                Nothing ->
                    class "page"
            ]
            (div [ class "loading-text" ] [ text "Loading..." ] :: inner)
        ]
    , Components.Footer.view
    ]
