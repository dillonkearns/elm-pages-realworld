module Auth exposing (User, beforeProtectedInit)

import Api.User
import ElmSpa.Page as ElmSpa
import Request exposing (Request)
import Route exposing (Route)
import Shared


type alias User =
    Api.User.User


beforeProtectedInit : Shared.Model -> Request -> ElmSpa.Protected User Route
beforeProtectedInit shared req =
    case shared.user of
        Just user ->
            ElmSpa.Provide user

        Nothing ->
            ElmSpa.RedirectTo Route.Index



--ElmSpa.RedirectTo Gen.Route.Login
