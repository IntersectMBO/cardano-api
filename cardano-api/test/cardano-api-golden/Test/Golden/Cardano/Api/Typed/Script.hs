{-# LANGUAGE TypeApplications #-}

module Test.Golden.Cardano.Api.Typed.Script
  ( test_golden_SimpleScriptV1_All
  , test_golden_SimpleScriptV1_Any
  , test_golden_SimpleScriptV1_MofN
  , test_golden_SimpleScriptV2_All
  , test_golden_SimpleScriptV2_Any
  , test_golden_SimpleScriptV2_MofN
  , test_roundtrip_SimpleScript_JSON
  , test_roundtrip_ScriptData
  , test_roundtrip_HashableScriptData_JSON
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Api.Era as L

import           Data.Aeson
import           System.FilePath ((</>))

import           Test.Gen.Cardano.Api.Typed

import           Hedgehog ((===))
import qualified Hedgehog as H
import           Hedgehog.Extras.Aeson
import           Test.Tasty (TestTree)
import           Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

exampleSimpleScriptV1_All :: SimpleScript
exampleSimpleScriptV1_All =
  RequireAllOf
    [ RequireSignature "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
    , RequireSignature "a687dcc24e00dd3caafbeb5e68f97ca8ef269cb6fe971345eb951756"
    , RequireSignature "0bd1d702b2e6188fe0857a6dc7ffb0675229bab58c86638ffa87ed6d"
    , RequireSignature "dd0044a26cf7d4491ecea720fda11afb59d5725b53afa605fdf695e6"
    , RequireSignature "cf223afe150cc8e89f11edaacbbd55b011ba44fbedef66fbd37d8c9d"
    , RequireSignature "372643e7ef4b41fd2649ada30a89d35cb90b7c14cb5de252e6ce6cb7"
    , RequireSignature "aa453dc184c5037d60e3fbbadb023f4a41bac112f249b76be9bb37ad"
    , RequireSignature "6b732c60c267bab894854d6dd57a04a94e603fcc4c36274c9ed75952"
    ]

exampleSimpleScriptV1_Any :: SimpleScript
exampleSimpleScriptV1_Any =
  RequireAnyOf
    [ RequireSignature "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
    , RequireSignature "4d780ed1bfc88cbd4da3f48de91fe728c3530d662564bf5a284b5321"
    , RequireSignature "3a94d6d4e786a3f5d439939cafc0536f6abc324fb8404084d6034bf8"
    , RequireSignature "b12e094d1db7c0fba5121f22db193d0060efed8be43654f861bb68ae"
    , RequireSignature "9be49d56442b4b8b16cab4e43e238bbdefc6c803d554c82fcd5facc3"
    , RequireSignature "622be5fab3b5c3f371a50a535e4d3349c942a98cecee93b24e2fd11d"
    ]

exampleSimpleScriptV1_MofN :: SimpleScript
exampleSimpleScriptV1_MofN =
  RequireMOf 2
    [ RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
    , RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
    , RequireSignature "b275b08c999097247f7c17e77007c7010cd19f20cc086ad99d398538"
    , RequireSignature "686024aecb5884d73a11b9ae4e63931112ba737e878d74638b78513a"
    ]


exampleSimpleScriptV2_All :: SimpleScript
exampleSimpleScriptV2_All =
  RequireAllOf
    [ RequireSignature "e09d36c79dec9bd1b3d9e152247701cd0bb860b5ebfd1de8abb6735a"
    , RequireTimeBefore (SlotNo 42)
    ]

exampleSimpleScriptV2_Any :: SimpleScript
exampleSimpleScriptV2_Any =
  RequireAnyOf
    [ RequireSignature "d92b712d1882c3b0f75b6f677e0b2cbef4fbc8b8121bb9dde324ff09"
    , RequireTimeAfter (SlotNo 42)
    ]

exampleSimpleScriptV2_MofN :: SimpleScript
exampleSimpleScriptV2_MofN =
  RequireMOf 1
    [ RequireSignature "2f3d4cf10d0471a1db9f2d2907de867968c27bca6272f062cd1c2413"
    , RequireSignature "f856c0c5839bab22673747d53f1ae9eed84afafb085f086e8e988614"
    , RequireTimeBefore (SlotNo 42)
    ]

goldenPath :: FilePath
goldenPath = "test/cardano-api-golden/files/golden/Script"

test_golden_SimpleScriptV1_All :: TestTree
test_golden_SimpleScriptV1_All =
  testProperty "golden SimpleScriptV1 All" $
    goldenTestJsonValuePretty exampleSimpleScriptV1_All (goldenPath </> "SimpleV1/all")

test_golden_SimpleScriptV1_Any :: TestTree
test_golden_SimpleScriptV1_Any =
  testProperty "golden SimpleScriptV1 Any" $
    goldenTestJsonValuePretty exampleSimpleScriptV1_Any (goldenPath </> "SimpleV1/any")

test_golden_SimpleScriptV1_MofN :: TestTree
test_golden_SimpleScriptV1_MofN =
  testProperty "golden SimpleScriptV1 MofN" $
    goldenTestJsonValuePretty exampleSimpleScriptV1_MofN (goldenPath </> "SimpleV1/atleast")

test_golden_SimpleScriptV2_All :: TestTree
test_golden_SimpleScriptV2_All =
  testProperty "golden SimpleScriptV2 All" $
    goldenTestJsonValuePretty exampleSimpleScriptV2_All (goldenPath </> "SimpleV2/all")

test_golden_SimpleScriptV2_Any :: TestTree
test_golden_SimpleScriptV2_Any =
  testProperty "golden SimpleScriptV2 Any" $
    goldenTestJsonValuePretty exampleSimpleScriptV2_Any (goldenPath </> "SimpleV2/any")

test_golden_SimpleScriptV2_MofN :: TestTree
test_golden_SimpleScriptV2_MofN =
  testProperty "golden SimpleScriptV2 MofN" $
    goldenTestJsonValuePretty exampleSimpleScriptV2_MofN (goldenPath </> "SimpleV2/atleast")

test_roundtrip_SimpleScript_JSON :: TestTree
test_roundtrip_SimpleScript_JSON =
  testProperty "roundtrip SimpleScript JSON" . H.property $ do
    mss <- H.forAll genSimpleScript
    H.tripping mss encode eitherDecode

test_roundtrip_ScriptData :: TestTree
test_roundtrip_ScriptData =
  testProperty "roundtrip ScriptData" . H.property $ do
    sData <- H.forAll genHashableScriptData
    sData === fromAlonzoData (toAlonzoData @L.Alonzo sData)

test_roundtrip_HashableScriptData_JSON :: TestTree
test_roundtrip_HashableScriptData_JSON =
  testProperty "roundtrip HashableScriptData" . H.property $ do
    sData <- H.forAll genHashableScriptData
    H.tripping sData scriptDataToJsonDetailedSchema scriptDataFromJsonDetailedSchema
