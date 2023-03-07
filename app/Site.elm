module Site exposing (config)

import BackendTask
import SiteConfig exposing (SiteConfig)


config : SiteConfig
config =
    { canonicalUrl = "https://elm-pages-realworld.netlify.app"
    , head = BackendTask.succeed []
    }
