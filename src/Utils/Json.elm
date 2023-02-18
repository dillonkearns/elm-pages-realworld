module Utils.Json exposing
    ( record
    , withDefault
    , withField
    )

import Json.Decode as Json



-- DECODING RECORDS


record : fn -> Json.Decoder fn
record =
    Json.succeed


withField :
    String
    -> Json.Decoder field
    -> Json.Decoder (field -> value)
    -> Json.Decoder value
withField str decoder =
    apply (Json.field str decoder)


withDefault : value -> Json.Decoder value -> Json.Decoder value
withDefault fallback decoder =
    Json.maybe decoder |> Json.map (Maybe.withDefault fallback)



-- ENCODING
-- INTERNALS


apply : Json.Decoder field -> Json.Decoder (field -> value) -> Json.Decoder value
apply =
    Json.map2 (|>)
