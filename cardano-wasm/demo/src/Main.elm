module Main exposing (main)

{-| Placeholder page: proves the demo build pipeline and that the cardano-wasm
engine loads in the browser (web/ports.js only starts this application after
`initialise()` has succeeded). The wallet application itself follows.
-}

import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (class, style)


main : Html msg
main =
    div [ class "grid" ]
        [ div [ class "col" ] []
        , div [ class "col" ]
            [ div [ class "card", style "margin-top" "40px" ]
                [ h3 [] [ text "cardano-wasm wallet demo" ]
                , p [] [ text "✓ the cardano-wasm engine loaded successfully in your browser." ]
                , p [ class "muted small" ] [ text "The wallet application will appear here as it is built up in subsequent changes." ]
                ]
            ]
        , div [ class "col" ] []
        ]
