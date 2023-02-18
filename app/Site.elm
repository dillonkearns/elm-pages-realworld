module Site exposing (config)

import BackendTask exposing (BackendTask)
import SiteConfig exposing (SiteConfig)


config : SiteConfig
config =
    { canonicalUrl = "https://elm-pages.com" -- TODO change canonical URL
    , head = BackendTask.succeed []
    }
