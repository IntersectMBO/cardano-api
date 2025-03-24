import Cardano.Api

main :: IO ()
main =
  print $ defaultTxBodyContent ShelleyBasedEraConway
