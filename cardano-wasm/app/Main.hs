module Main where

import Cardano.Wasm.Api.Info (apiInfo)
import Cardano.Wasm.Api.InfoToTypeScript (apiInfoToTypeScriptFile)
import Cardano.Wasm.Api.TypeScriptDefs (writeTypeScriptToDir)

import Options.Applicative

newtype CmdArgs
  = CmdArgs {outputDir :: String}

parser :: Parser CmdArgs
parser =
  CmdArgs
    <$> strOption
      ( mconcat
          [ long "output-dir"
          , short 'o'
          , metavar "OUTPUT_DIR"
          , help "Output directory for the TypeScript declaration files (it must exist)"
          ]
      )

main :: IO ()
main = do
  cmdArgs <- execParser (info (parser <**> helper) fullDesc)
  mapM_ (writeTypeScriptToDir (outputDir cmdArgs)) (apiInfoToTypeScriptFile apiInfo)
