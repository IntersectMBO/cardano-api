module Bech32 exposing (bech32ToHex)

{-| PROVISIONAL bech32 → base16 decoder.

The delegation certificate needs the pool id in base16, while the pool picker
carries the bech32 id from the provider. Blockfrost already returns the hex
directly, so this decoder is only a fallback for pools missing from the loaded
set. It performs no checksum validation. Ideally cardano-wasm would expose this
conversion and this module would disappear.

-}

import Bitwise
import Hex


bech32Charset : String
bech32Charset =
    "qpzry9x8gf2tvdw0s3jn54khce6mua7l"


{-| Decode a bech32 string (e.g. "pool1…") to the base16 of its data payload,
dropping the human-readable prefix and the 6-symbol checksum.
No checksum validation — provisional.
-}
bech32ToHex : String -> Maybe String
bech32ToHex input =
    let
        chars =
            String.toList (String.toLower input)

        sep =
            lastIndexOfChar '1' chars 0 -1

        vals =
            List.drop (sep + 1) chars |> List.map charIndex
    in
    if sep < 0 || List.any (\v -> v < 0) vals || List.length vals < 6 then
        Nothing

    else
        convertBits 5 8 False (List.take (List.length vals - 6) vals)
            |> Maybe.map Hex.bytesToHex


charIndex : Char -> Int
charIndex c =
    indexInList c (String.toList bech32Charset) 0


indexInList : Char -> List Char -> Int -> Int
indexInList c cs i =
    case cs of
        [] ->
            -1

        x :: rest ->
            if x == c then
                i

            else
                indexInList c rest (i + 1)


lastIndexOfChar : Char -> List Char -> Int -> Int -> Int
lastIndexOfChar c cs i best =
    case cs of
        [] ->
            best

        x :: rest ->
            lastIndexOfChar c
                rest
                (i + 1)
                (if x == c then
                    i

                 else
                    best
                )


{-| Regroup a bit stream from `from`-bit symbols to `to`-bit symbols (the standard
bech32 5→8 bit conversion). Without padding, leftover bits must be zero.
-}
convertBits : Int -> Int -> Bool -> List Int -> Maybe (List Int)
convertBits from to pad data =
    let
        maxv =
            Bitwise.shiftLeftBy to 1 - 1

        drain acc bits out =
            if bits >= to then
                drain acc (bits - to) (Bitwise.and maxv (Bitwise.shiftRightBy (bits - to) acc) :: out)

            else
                ( bits, out )

        step v ( acc, bits, out ) =
            let
                acc1 =
                    Bitwise.or (Bitwise.shiftLeftBy from acc) v

                ( bits2, out2 ) =
                    drain acc1 (bits + from) out

                mask =
                    Bitwise.shiftLeftBy bits2 1 - 1
            in
            ( Bitwise.and mask acc1, bits2, out2 )

        ( finalAcc, finalBits, revOut ) =
            List.foldl step ( 0, 0, [] ) data
    in
    if pad then
        Just
            (List.reverse
                (if finalBits > 0 then
                    Bitwise.and maxv (Bitwise.shiftLeftBy (to - finalBits) finalAcc) :: revOut

                 else
                    revOut
                )
            )

    else if finalBits >= from || Bitwise.and maxv (Bitwise.shiftLeftBy (to - finalBits) finalAcc) /= 0 then
        Nothing

    else
        Just (List.reverse revOut)
