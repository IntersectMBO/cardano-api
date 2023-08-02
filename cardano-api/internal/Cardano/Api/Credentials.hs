{-# LANGUAGE DataKinds #-}

module Cardano.Api.Credentials where

import           Cardano.Api.Certificate
import           Cardano.Api.Governance.Poll (Hash (DRepKeyHash))
import           Cardano.Api.Keys.Class
import           Cardano.Api.ReexposeLedger

-- In Conway era onways, DRep and Committee members are registered on chain via their credentials.
-- I.E they do not have corresponding addresses.
-- NB: DRep and Committee keys cannot be converted to addresses because addresses
-- only have notions of payment and stake credentials, not voting credentials.


-- ----------------------------------------------------------------------------
-- Credential generation
--

genDRepCredentialAndKeyHash :: IO (Credential 'Voting StandardCrypto, KeyHash 'Voting StandardCrypto)
genDRepCredentialAndKeyHash = do
  skey <- generateSigningKey AsDRepKey
  let vkey = getVerificationKey skey
      DRepKeyHash kHash = verificationKeyHash vkey
      ledgerKeyCredential = KeyHashObj kHash
  return (ledgerKeyCredential, kHash)

