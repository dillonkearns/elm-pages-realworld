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
        , div [ class "page" ]
            (case app.transition of
                Just _ ->
                    [ text "Loading..."
                    ]

                _ ->
                    inner
            )
        ]
    , Components.Footer.view
    ]
