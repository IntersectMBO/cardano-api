module Format exposing (ada, adaToLovelace, amountError, lovelaceToAda, orDefault, shorten)

{-| String formatting and parsing: ada ↔ lovelace, abbreviation, small text helpers.
-}


{-| Parse a user-typed ADA amount ("1.5" → 1500000 lovelace).
-}
adaToLovelace : String -> Maybe Int
adaToLovelace s =
    String.toFloat (String.trim s)
        |> Maybe.map (\f -> round (f * 1.0e6))


{-| True when a typed amount is non-empty but not a valid positive number (e.g. "1,5").
-}
amountError : String -> Bool
amountError s =
    if String.trim s == "" then
        False

    else
        case adaToLovelace s of
            Just n ->
                n <= 0

            Nothing ->
                True


lovelaceToAda : Int -> String
lovelaceToAda l =
    let
        sign =
            if l < 0 then
                "-"

            else
                ""

        a =
            abs l

        whole =
            a // 1000000

        frac =
            String.padLeft 6 '0' (String.fromInt (modBy 1000000 a)) |> stripTrailingZeros
    in
    sign
        ++ String.fromInt whole
        ++ (if frac == "" then
                ""

            else
                "." ++ frac
           )


ada : Int -> String
ada l =
    lovelaceToAda l ++ " ₳"


stripTrailingZeros : String -> String
stripTrailingZeros s =
    String.foldr
        (\c ( acc, trimming ) ->
            if trimming && c == '0' then
                ( acc, True )

            else
                ( String.cons c acc, False )
        )
        ( "", True )
        s
        |> Tuple.first


{-| Abbreviate long identifiers (addresses, keys, hashes) for display.
-}
shorten : String -> String
shorten s =
    if String.length s > 22 then
        String.left 12 s ++ "…" ++ String.right 6 s

    else
        s


orDefault : String -> String -> String
orDefault d s =
    if String.trim s == "" then
        d

    else
        s
