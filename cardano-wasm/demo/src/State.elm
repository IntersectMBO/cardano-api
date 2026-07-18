module State exposing
    ( addWallet
    , aliasOf
    , emptyRestoreForm
    , getWallet
    , init
    , log
    , mapWallet
    , setRestorePay
    , setRestoreStake
    , toastNow
    , toggleRestore
    )

{-| Everything about the Model: the initial state, derived queries (what the view
and update read), and the small pure updaters. No commands except the toast timer.
-}

import Process
import Task
import Types exposing (..)



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { network = Mainnet
      , wallets = []
      , nextWid = 1
      , modal = NoModal
      , restore = emptyRestoreForm
      , console =
            [ LogLine LogInfo "cardano-wasm loaded · post-link module ready" ]
      , toast = Nothing
      , toastSeq = 0
      }
    , Cmd.none
    )


emptyRestoreForm : RestoreForm
emptyRestoreForm =
    { open = False, paymentSkey = "", stakeSkey = "" }



-- WALLETS


getWallet : WalletId -> Model -> Maybe Wallet
getWallet wid model =
    List.filter (\w -> w.id == wid) model.wallets |> List.head


mapWallet : WalletId -> (Wallet -> Wallet) -> Model -> Model
mapWallet wid f model =
    { model
        | wallets =
            List.map
                (\w ->
                    if w.id == wid then
                        f w

                    else
                        w
                )
                model.wallets
    }


aliasOf : WalletId -> Model -> String
aliasOf wid model =
    getWallet wid model |> Maybe.map .alias |> Maybe.withDefault "?"


avatarColors : List String
avatarColors =
    [ "#3b73ff", "#33d17a", "#f6b73c", "#ff6b6b", "#a06bff", "#19cdd7", "#ff9ed6", "#5ee89c" ]


addWallet : GenPayload -> Model -> Model
addWallet p model =
    let
        color =
            List.drop (modBy (List.length avatarColors) (model.nextWid - 1)) avatarColors
                |> List.head
                |> Maybe.withDefault "#3b73ff"

        w =
            { id = model.nextWid
            , alias = "Wallet " ++ String.fromInt model.nextWid
            , address = p.address
            , keys = p.keys
            , expanded = True
            , color = color
            }
    in
    { model | wallets = model.wallets ++ [ w ], nextWid = model.nextWid + 1 }



-- CONSOLE & TOAST


log : LogLevel -> String -> Model -> Model
log level text model =
    let
        entries =
            model.console ++ [ LogLine level text ]
    in
    -- keep the last 200 lines only
    { model | console = List.drop (List.length entries - 200) entries }


{-| Show a toast and schedule its dismissal; the sequence number ignores stale timers.
-}
toastNow : String -> Model -> ( Model, Cmd Msg )
toastNow text model =
    let
        seq =
            model.toastSeq + 1
    in
    ( { model | toast = Just text, toastSeq = seq }
    , Process.sleep 1900 |> Task.perform (\_ -> ClearToast seq)
    )



-- SMALL FORM UPDATERS


toggleRestore : RestoreForm -> RestoreForm
toggleRestore r =
    { r | open = not r.open }


setRestorePay : String -> RestoreForm -> RestoreForm
setRestorePay s r =
    { r | paymentSkey = s }


setRestoreStake : String -> RestoreForm -> RestoreForm
setRestoreStake s r =
    { r | stakeSkey = s }
