module Format exposing (ada, adaToLovelace, amountError, digits, lovelaceToAda, orDefault, shorten)

{-| String formatting and parsing: ada ↔ lovelace, abbreviation, small text helpers.
-}


{-| Parse a user-typed ADA amount ("1.5" → 1500000 lovelace).
Exact decimal parsing — the digits are scaled as integers, so amounts never
pick up float rounding. At most 6 decimals, no sign, no exponent.
-}
adaToLovelace : String -> Maybe Int
adaToLovelace s =
    case String.split "." (String.trim s) of
        [ whole ] ->
            Maybe.map ((*) 1000000) (digits whole)

        [ whole, frac ] ->
            if frac == "" || String.length frac > 6 then
                Nothing

            else
                Maybe.map2 (\w f -> w * 1000000 + f * 10 ^ (6 - String.length frac))
                    (digits whole)
                    (digits frac)

        _ ->
            Nothing


{-| String.toInt restricted to plain digit runs (rejects signs and exponents).
Also the parser for Blockfrost's decimal quantity strings.
-}
digits : String -> Maybe Int
digits str =
    if str /= "" && String.all Char.isDigit str then
        String.toInt str

    else
        Nothing


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

        rest =
            modBy 1000000 a

        whole =
            -- Elm's // truncates through a 32-bit cast, which garbles whole-ADA
            -- counts above ~2.1e9. Subtracting the remainder first makes the
            -- float division exact, so floor is safe for the full Int range.
            floor (toFloat (a - rest) / 1.0e6)

        frac =
            String.padLeft 6 '0' (String.fromInt rest) |> stripTrailingZeros
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
