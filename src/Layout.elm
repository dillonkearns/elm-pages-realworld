module Layout exposing (view)

import Components.Footer
import Components.Navbar
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Encode
import Pages.Msg
import Route


view user inner =
    [ div [ class "layout" ]
        [ Components.Navbar.view
            { user = user
            , currentRoute = Route.Index
            }
        , div [ class "page" ] inner
        ]
    , Components.Footer.view
    ]
