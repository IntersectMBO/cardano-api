module Net exposing
    ( blockfrostBase
    , cliFlag
    , expectedNetKind
    , explorerTx
    , faucetUrl
    , netMagic
    , netName
    , netTag
    )

{-| Static tables for the three networks. Pure data — no logic lives here.
-}

import Types exposing (..)



-- NETWORKS


netName : Network -> String
netName n =
    case n of
        Mainnet ->
            "Mainnet"

        Preprod ->
            "Preprod"

        Preview ->
            "Preview"


{-| Lowercase identifier used on the JS side of the ports.
-}
netTag : Network -> String
netTag n =
    case n of
        Mainnet ->
            "mainnet"

        Preprod ->
            "preprod"

        Preview ->
            "preview"


netMagic : Network -> String
netMagic n =
    case n of
        Mainnet ->
            "764824073"

        Preprod ->
            "1"

        Preview ->
            "2"


blockfrostBase : Network -> String
blockfrostBase n =
    case n of
        Mainnet ->
            "https://cardano-mainnet.blockfrost.io/api/v0"

        Preprod ->
            "https://cardano-preprod.blockfrost.io/api/v0"

        Preview ->
            "https://cardano-preview.blockfrost.io/api/v0"


explorerTx : Network -> String
explorerTx n =
    case n of
        Mainnet ->
            "https://cardanoscan.io/"

        Preprod ->
            "https://preprod.cardanoscan.io/"

        Preview ->
            "https://preview.cardanoscan.io/"


faucetUrl : Network -> Maybe String
faucetUrl n =
    case n of
        Mainnet ->
            Nothing

        _ ->
            Just "https://docs.cardano.org/cardano-testnets/tools/faucet/"


cliFlag : Network -> String
cliFlag n =
    case n of
        Mainnet ->
            "--mainnet"

        Preprod ->
            "--testnet-magic 1"

        Preview ->
            "--testnet-magic 2"


{-| The network kind an address must have to be usable on the selected network.
Addresses only encode mainnet-vs-testnet, so preprod and preview both map to TestKind.
-}
expectedNetKind : Network -> NetKind
expectedNetKind n =
    case n of
        Mainnet ->
            MainKind

        _ ->
            TestKind
