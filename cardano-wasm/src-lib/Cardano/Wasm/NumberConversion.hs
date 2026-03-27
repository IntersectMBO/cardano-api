module Cardano.Wasm.NumberConversion
  ( integerToNatural
  )
where

import Cardano.Wasm.ExceptionHandling (throwError)

import Numeric.Natural (Natural)

integerToNatural :: Integer -> IO Natural
integerToNatural i
  | i < 0 = throwError $ "Expected natural number, but got a negative value: " ++ show i
  | otherwise = return $ fromIntegral i
