module Bech32 exposing (bech32ToHex)

{-| Bech32 → base16 decoder for pool ids.

The delegation certificate needs the pool id in base16; the picker pins
Blockfrost's hex at pick time, so the application no longer performs this
conversion itself. The module is kept as the safe path should a manual pool-id
entry ever be added: it validates fully (checksum, `pool` prefix, 28-byte
payload, no mixed case), so a mistyped id can never silently decode to a
different pool. Ideally cardano-wasm would expose this conversion and this
module would disappear.

-}

import Bitwise
import Hex


bech32Charset : String
bech32Charset =
    "qpzry9x8gf2tvdw0s3jn54khce6mua7l"


{-| Decode a `pool1…` id to the base16 of its data payload, dropping the
prefix and the 6-symbol checksum. Full BIP-173 validation: rejects mixed case,
a prefix other than `pool`, a bad checksum, and any payload that is not
exactly the 28 bytes of a pool key hash.
-}
bech32ToHex : String -> Maybe String
bech32ToHex input =
    let
        lower =
            String.toLower input

        chars =
            String.toList lower

        sep =
            lastIndexOfChar '1' chars 0 -1

        hrp =
            List.take sep chars

        vals =
            List.drop (sep + 1) chars |> List.map charIndex

        -- BIP-173: a string must be all-lowercase or all-uppercase
        mixedCase =
            input /= lower && input /= String.toUpper input
    in
    if mixedCase || sep < 0 || List.any (\v -> v < 0) vals || List.length vals < 6 then
        Nothing

    else if hrp /= String.toList "pool" || polymod (hrpExpand hrp ++ vals) /= 1 then
        Nothing

    else
        convertBits 5 8 False (List.take (List.length vals - 6) vals)
            |> Maybe.andThen
                (\bytes ->
                    if List.length bytes == 28 then
                        Just (Hex.bytesToHex bytes)

                    else
                        Nothing
                )


{-| The BIP-173 checksum: over `hrpExpand hrp ++ data` (data including the
6 checksum symbols) a valid bech32 string yields exactly 1.
-}
polymod : List Int -> Int
polymod values =
    let
        generator =
            [ 0x3B6A57B2, 0x26508E6D, 0x1EA119FA, 0x3D4233DD, 0x2A1462B3 ]

        step v chk =
            let
                b =
                    Bitwise.shiftRightZfBy 25 chk

                shifted =
                    Bitwise.xor (Bitwise.shiftLeftBy 5 (Bitwise.and chk 0x01FFFFFF)) v
            in
            List.foldl
                (\( i, g ) acc ->
                    if Bitwise.and (Bitwise.shiftRightZfBy i b) 1 == 1 then
                        Bitwise.xor acc g

                    else
                        acc
                )
                shifted
                (List.indexedMap Tuple.pair generator)
    in
    List.foldl step 1 values


hrpExpand : List Char -> List Int
hrpExpand hrp =
    let
        codes =
            List.map Char.toCode hrp
    in
    List.map (Bitwise.shiftRightBy 5) codes ++ [ 0 ] ++ List.map (Bitwise.and 31) codes


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
