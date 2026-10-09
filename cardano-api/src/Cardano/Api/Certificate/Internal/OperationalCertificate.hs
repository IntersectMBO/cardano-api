-- | Operational certificates
--
-- The certificate types and their decoding accessors now live in the
-- cardano-keys package; re-exported here for compatibility, together with the
-- issuing function this API adds on top of them.
module Cardano.Api.Certificate.Internal.OperationalCertificate
  ( OperationalCertificate (..)
  , OperationalCertificateIssueCounter (..)
  , Shelley.KESPeriod (..)
  , OperationalCertIssueError (..)
  , getHotKey
  , getKesPeriod
  , getOpCertCount
  , issueOperationalCertificate

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Error
import Cardano.Api.Key.Internal
import Cardano.Api.Key.Internal.Class
import Cardano.Api.Key.Internal.Praos
import Cardano.Api.Tx.Internal.Sign

import Cardano.Crypto.DSIGN qualified as DSIGN
import Cardano.Keys.OperationalCertificate
import Cardano.Ledger.Keys qualified as Shelley
import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Protocol.TPraos.OCert qualified as Shelley

import GHC.Stack (HasCallStack)

data OperationalCertIssueError
  = -- | The stake pool verification key expected for the
    -- 'OperationalCertificateIssueCounter' does not match the signing key
    -- supplied for signing.
    --
    -- Order: pool vkey expected, pool skey supplied
    OperationalCertKeyMismatch
      (VerificationKey StakePoolKey)
      (VerificationKey StakePoolKey)
  deriving Show

instance Error OperationalCertIssueError where
  prettyError (OperationalCertKeyMismatch _counterKey _signingKey) =
    "Key mismatch: the signing key does not match the one that goes with the counter"

-- TODO: include key ids

issueOperationalCertificate
  :: HasCallStack
  => VerificationKey KesKey
  -> Either
       AnyStakePoolSigningKey
       (SigningKey GenesisDelegateExtendedKey)
  -- TODO: this may be better with a type that
  -- captured the three (four?) choices, stake pool
  -- or genesis delegate, extended or normal.
  -> Shelley.KESPeriod
  -> OperationalCertificateIssueCounter
  -> Either
       OperationalCertIssueError
       ( OperationalCertificate
       , OperationalCertificateIssueCounter
       )
issueOperationalCertificate
  (KesVerificationKey kesVKey)
  skey
  kesPeriod
  (OperationalCertificateIssueCounter counter poolVKey)
    | poolVKey /= poolVKey' =
        Left (OperationalCertKeyMismatch poolVKey poolVKey')
    | otherwise =
        Right
          ( OperationalCertificate ocert poolVKey
          , OperationalCertificateIssueCounter (succ counter) poolVKey
          )
   where
    castAnyStakePoolSigningKeyToNormalVerificationKey
      :: AnyStakePoolSigningKey
      -> VerificationKey StakePoolKey
    castAnyStakePoolSigningKeyToNormalVerificationKey anyStakePoolSKey =
      case anyStakePoolSigningKeyToVerificationKey anyStakePoolSKey of
        AnyStakePoolNormalVerificationKey normalStakePoolVKey -> normalStakePoolVKey
        AnyStakePoolExtendedVerificationKey extendedStakePoolVKey ->
          castVerificationKey extendedStakePoolVKey

    poolVKey' :: VerificationKey StakePoolKey
    poolVKey' =
      either
        castAnyStakePoolSigningKeyToNormalVerificationKey
        (convert . getVerificationKey)
        skey
     where
      convert
        :: VerificationKey GenesisDelegateExtendedKey
        -> VerificationKey StakePoolKey
      convert =
        ( castVerificationKey
            :: VerificationKey GenesisDelegateKey
            -> VerificationKey StakePoolKey
        )
          . ( castVerificationKey
                :: VerificationKey GenesisDelegateExtendedKey
                -> VerificationKey GenesisDelegateKey
            )

    ocert :: Shelley.OCert StandardCrypto
    ocert = Shelley.OCert kesVKey counter kesPeriod signature

    signature
      :: DSIGN.SignedDSIGN
           Shelley.DSIGN
           (Shelley.OCertSignable StandardCrypto)
    signature =
      makeShelleySignature
        (Shelley.OCertSignable kesVKey counter kesPeriod)
        skey'
     where
      skey' :: ShelleySigningKey
      skey' = case skey of
        Left (AnyStakePoolNormalSigningKey (StakePoolSigningKey poolSKey)) ->
          ShelleyNormalSigningKey poolSKey
        Left
          ( AnyStakePoolExtendedSigningKey
              (StakePoolExtendedSigningKey poolExtendedSKey)
            ) ->
            ShelleyExtendedSigningKey poolExtendedSKey
        Right (GenesisDelegateExtendedSigningKey delegSKey) ->
          ShelleyExtendedSigningKey delegSKey
