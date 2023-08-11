module Cardano.Api.Domain.Errors.ProtocolParametersError
  ( ProtocolParametersError(..)
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Error

data ProtocolParametersError
  = PParamsErrorMissingMinUTxoValue !AnyCardanoEra
  | PParamsErrorMissingAlonzoProtocolParameter
  deriving (Show)

instance Error ProtocolParametersError where
  displayError (PParamsErrorMissingMinUTxoValue (AnyCardanoEra era)) = mconcat
    [ "The " <> show era <> " protocol parameters value is missing the following "
    , "field: MinUTxoValue. Did you intend to use a " <> show era <> " protocol "
    , "parameters value?"
    ]
  displayError PParamsErrorMissingAlonzoProtocolParameter = mconcat
    [ "The Alonzo era protocol parameters in use is missing one or more of the "
    , "following fields: UTxOCostPerWord, CostModels, Prices, MaxTxExUnits, "
    , "MaxBlockExUnits, MaxValueSize, CollateralPercent, MaxCollateralInputs. Did "
    , "you intend to use an Alonzo era protocol parameters value?"
    ]
