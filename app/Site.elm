module Site exposing (config)

import BackendTask exposing (BackendTask)
import SiteConfig exposing (SiteConfig)


config : SiteConfig
config =
    { canonicalUrl = "https://elm-pages-realworld.netlify.app"
    , head = BackendTask.succeed []
    }
