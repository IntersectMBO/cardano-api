module Cardano.Api.Scripts.Plutus where


import           Cardano.Api.Eon.BabbageEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Case
import           Cardano.Api.Eras.Core
import           Cardano.Api.Error
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ScriptData
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.SerialiseUsing
import           Cardano.Api.TxIn
import           Cardano.Api.Utils (failEitherWith)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Allegra.Scripts as Allegra
import qualified Cardano.Ledger.Allegra.Scripts as Timelock
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Babbage.Scripts as Babbage
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Binary as Binary (decCBOR, decodeFullAnnotator)
import qualified Cardano.Ledger.Conway.Scripts as Conway
import           Cardano.Ledger.Core (Era (EraCrypto))
import qualified Cardano.Ledger.Core as Ledger
import           Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Keys as Shelley
import qualified Cardano.Ledger.Plutus.Language as Plutus
import qualified Cardano.Ledger.Shelley.Scripts as Shelley
import qualified PlutusLedgerApi.Test.Examples as Plutus

import           Control.Applicative
import           Control.Monad
import           Data.Aeson (Value (..), object, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import           Data.Either.Combinators (maybeToRight)
import           Data.Functor
import           Data.Scientific (toBoundedInteger)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import           Data.Typeable (Typeable)
import           Data.Vector (Vector)
import           GHC.Exts (IsList (..))
import           GHC.TypeLits