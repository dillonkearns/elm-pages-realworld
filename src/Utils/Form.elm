module Utils.Form exposing (inProgressText)


inProgressText : { formState | isTransitioning : Bool } -> String -> String
inProgressText formState string =
    if formState.isTransitioning then
        string ++ "..."

    else
        string ++ ""
