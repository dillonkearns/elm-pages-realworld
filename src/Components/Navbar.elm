module Components.Navbar exposing (view)

import Api.Token exposing (Token)
import Api.User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Route exposing (Route)


view :
    { user : Maybe User
    , currentRoute : Route
    , onSignOut : msg
    }
    -> Html msg
view options =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ Route.Index |> Route.link [ class "navbar-brand" ] [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                case options.user of
                    Just _ ->
                        List.concat
                            [ List.map (viewLink options.currentRoute) <|
                                [ ( "Home", Route.Index )

                                --, ( "New Article", Route.Editor )
                                --, ( "Settings", Route.Settings )
                                ]
                            , [ li [ class "nav-item" ]
                                    [ a
                                        [ class "nav-link"
                                        , Events.onClick options.onSignOut
                                        ]
                                        [ text "Sign out" ]
                                    ]
                              ]
                            ]

                    Nothing ->
                        List.map (viewLink options.currentRoute) <|
                            [ ( "Home", Route.Index )
                            , ( "Sign in", Route.Login )
                            , ( "Sign up", Route.Register )
                            ]
            ]
        ]


viewLink : Route -> ( String, Route ) -> Html msg
viewLink currentRoute ( label, route ) =
    li [ class "nav-item" ]
        [ route
            |> Route.link
                [ class "nav-link"
                , classList [ ( "active", currentRoute == route ) ]
                ]
                [ text label ]
        ]
