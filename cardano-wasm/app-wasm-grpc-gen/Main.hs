{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, sortOn)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import GHC.Generics
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)
import Text.Casing (camel, pascal)

-- =============================================================================
-- 1. JSON AST Definitions (Matching bundle.json structure)
-- =============================================================================

newtype Bundle = Bundle {nested :: Maybe (HM.HashMap T.Text Namespace)}
  deriving (Show, Generic)

instance FromJSON Bundle

data Namespace = Namespace
  { nestedNS :: Maybe (HM.HashMap T.Text Namespace) -- "nested"
  , fields :: Maybe (HM.HashMap T.Text Field) -- For Messages
  , oneofs :: Maybe (HM.HashMap T.Text OneOf) -- For OneOfs
  , methods :: Maybe (HM.HashMap T.Text Method) -- For Services
  , values :: Maybe (HM.HashMap T.Text Int) -- For Enums
  }
  deriving (Show, Generic)

instance FromJSON Namespace where
  parseJSON :: Value -> Parser Namespace
  parseJSON = withObject "Namespace" $ \v ->
    Namespace
      <$> v .:? "nested"
      <*> v .:? "fields"
      <*> v .:? "oneofs"
      <*> v .:? "methods"
      <*> v .:? "values"

data Field = Field
  { typeName :: T.Text -- "type": "string", "MyMessage", etc.
  , rule :: Maybe T.Text -- "repeated" or Nothing
  , fieldId :: Int -- Renamed from 'id' to avoid Prelude clash
  }
  deriving (Show, Generic)

instance FromJSON Field where
  parseJSON = withObject "Field" $ \v ->
    Field
      <$> v .: "type"
      <*> v .:? "rule"
      <*> v .: "id" -- Map JSON "id" to Haskell "fieldId"

newtype OneOf = OneOf
  { oneof :: [T.Text] -- List of field names belonging to this oneof
  }
  deriving (Show, Generic)

instance FromJSON OneOf

data Method = Method
  { requestType :: T.Text
  , responseType :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Method

-- =============================================================================
-- 2. Main Entry Point
-- =============================================================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonPath, outDir, rootModule] -> do
      jsonContent <- BSL.readFile jsonPath
      case eitherDecode jsonContent of
        Left err -> error $ "JSON Decode Error: " ++ err
        Right bundle -> do
          putStrLn $ "Parsing bundle from: " ++ jsonPath
          putStrLn $ "Generating Haskell to: " ++ outDir

          -- 1. Generate the Base Client Module (The Interface)
          generateBaseModule outDir rootModule

          -- 2. Recursive Generation of Types & Services
          generateNamespace outDir rootModule [] (nested bundle)

          putStrLn "Done."
    _ -> do
      exeName <- getProgName
      hPutStrLn stderr $ T.pack ("Usage: " ++ exeName ++ " <bundle.json> <output_dir> <root_module_name>")
      exitWith (ExitFailure 1)

-- =============================================================================
-- 3. Code Generation Logic
-- =============================================================================

-- | Generates the GrpcClient.hs module which defines the Executor type
generateBaseModule :: String -> String -> IO ()
generateBaseModule outDir rootMod = do
  let content =
        unlines
          [ "{-# LANGUAGE OverloadedStrings #-}"
          , "module " ++ rootMod ++ ".GrpcClient where"
          , ""
          , "import Data.Text (Text)"
          , ""
          , "-- | The bridge function signature."
          , "-- Arguments: HostURL -> ServiceName -> MethodName -> JSONPayload -> IO JSONResponse"
          , "type GrpcExecutor = String -> String -> String -> String -> IO String"
          ]
  writeFileWithDir (outDir </> "GrpcClient.hs") content

-- | Recursive traversal
generateNamespace :: String -> String -> [T.Text] -> Maybe (HM.HashMap T.Text Namespace) -> IO ()
generateNamespace _ _ _ Nothing = return ()
generateNamespace outDir rootMod path (Just items) = do
  mapM_ (processItem outDir rootMod path) (HM.toList items)

processItem :: String -> String -> [T.Text] -> (T.Text, Namespace) -> IO ()
processItem outDir rootMod parentPath (name, ns) = do
  let currentPath = parentPath ++ [T.pack (pascal (T.unpack name))]

  -- A namespace can be multiple things at once (e.g. Message AND containing nested types)

  -- 1. Recurse first (for nested types)
  generateNamespace outDir rootMod currentPath (nestedNS ns)

  -- 2. Check if it's a Message, Enum, or Service and generate file
  let isMessage = isJust $ fields ns
  let isEnum = isJust $ values ns
  let isService = isJust $ methods ns

  when (isMessage || isEnum || isService) $
    writeModule outDir rootMod currentPath ns

writeModule :: String -> String -> [T.Text] -> Namespace -> IO ()
writeModule outDir rootMod pathParts ns = do
  let moduleNameStr = T.unpack $ T.intercalate "." pathParts
  let fullModName = rootMod ++ "." ++ moduleNameStr
  let filePath = outDir </> foldl (</>) "" (map T.unpack pathParts) ++ ".hs"

  let imports =
        [ "import Data.Aeson"
        , "import Data.Aeson.Types (emptyObject, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, omitNothingFields)"
        , "import GHC.Generics"
        , "import Data.Text (Text)"
        , "import qualified Data.Map as Map"
        , "import " ++ rootMod ++ ".GrpcClient"
        ]

  let body = case (fields ns, values ns, methods ns) of
        (Just fs, _, _) -> generateMessage (last pathParts) fs (oneofs ns)
        (_, Just vs, _) -> generateEnum (last pathParts) vs
        (_, _, Just ms) -> generateService rootMod pathParts (last pathParts) ms
        _ -> ""

  let fileContent =
        unlines $
          [ "{-# LANGUAGE DeriveGeneric #-}"
          , "{-# LANGUAGE OverloadedStrings #-}"
          , "{-# LANGUAGE DuplicateRecordFields #-}"
          , "module " ++ fullModName ++ " where"
          , ""
          ]
            ++ imports
            ++ ["", body]

  writeFileWithDir filePath fileContent
  putStrLn $ "  -> " ++ filePath

-- =============================================================================
-- 4. Type Generators
-- =============================================================================

generateMessage :: T.Text -> HM.HashMap T.Text Field -> Maybe (HM.HashMap T.Text OneOf) -> String
generateMessage name fieldsMap _ =
  let msgName = pascal (T.unpack name)

      -- Sort fields by ID for stability (Fixed: use sortOn and fieldId)
      sortedFields = sortOn (\(_, f) -> fieldId f) (HM.toList fieldsMap)

      mkField :: (T.Text, Field) -> String
      mkField (fName, f) =
        let fType = mapType (rule f) (typeName f)
            fNameStr = camel msgName ++ pascal (T.unpack fName)
         in "    , " ++ fNameStr ++ " :: " ++ fType

      fieldLines = case sortedFields of
        [] -> []
        ((n, f) : fs) ->
          let firstLine =
                "    { " ++ camel msgName ++ pascal (T.unpack n) ++ " :: " ++ mapType (rule f) (typeName f)
           in firstLine : map mkField fs
   in if null fieldLines
        then
          unlines
            [ "data " ++ msgName ++ " = " ++ msgName
            , "    deriving (Show, Eq, Generic)"
            , "instance FromJSON " ++ msgName ++ " where parseJSON _ = return " ++ msgName
            , "instance ToJSON " ++ msgName ++ " where toJSON _ = emptyObject"
            ]
        else
          unlines
            [ "data " ++ msgName ++ " = " ++ msgName
            , unlines fieldLines
            , "    } deriving (Show, Eq, Generic)"
            , ""
            , "-- JSON Instances"
            , "instance FromJSON " ++ msgName ++ " where"
            , "  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop "
                ++ show (length msgName)
                ++ " }"
            , "instance ToJSON " ++ msgName ++ " where"
            , "  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop "
                ++ show (length msgName)
                ++ ", omitNothingFields = True }"
            ]

generateEnum :: T.Text -> HM.HashMap T.Text Int -> String
generateEnum name valuesMap =
  let enumName = pascal (T.unpack name)
      -- Sort by value (Fixed: using sortOn)
      sortedVals = sortOn snd (HM.toList valuesMap)
      mkCons (k, _) = pascal (T.unpack k)
      constructors = map mkCons sortedVals
   in unlines
        [ "data " ++ enumName
        , "  = " ++ intercalate "\n  | " constructors
        , "  deriving (Show, Eq, Enum, Bounded, Generic)"
        , ""
        , "instance FromJSON " ++ enumName
        , "instance ToJSON " ++ enumName
        ]

generateService :: String -> [T.Text] -> T.Text -> HM.HashMap T.Text Method -> String
generateService _rootMod pathParts svcName methodsMap =
  let
    pkgName = T.intercalate "." $ init pathParts
    fullServiceName = pkgName <> "." <> svcName

    mkMethod :: (T.Text, Method) -> String
    mkMethod (mName, m) =
      let funcName = camel (T.unpack mName)
          reqT = pascal (T.unpack (requestType m))
          resT = pascal (T.unpack (responseType m))
       in unlines
            [ "-- | Call " ++ T.unpack mName
            , funcName ++ " :: GrpcExecutor -> " ++ reqT ++ " -> IO (Either String " ++ resT ++ ")"
            , funcName ++ " exec req = do"
            , "  let jsonReq = decodeUtf8 $ BSL.toStrict $ encode req"
            , "  resp <- exec service method (T.unpack jsonReq)"
            , "  return $ eitherDecode (BSL.fromStrict $ T.encodeUtf8 $ T.pack resp)"
            , "  where"
            , "    service = \"" ++ T.unpack fullServiceName ++ "\""
            , "    method = \"" ++ T.unpack mName ++ "\""
            ]
   in
    unlines
      [ "import Data.Text.Encoding (encodeUtf8, decodeUtf8)"
      , "import qualified Data.ByteString.Lazy as BSL"
      , "import qualified Data.Text as T"
      , ""
      , concatMap mkMethod (HM.toList methodsMap)
      ]

-- =============================================================================
-- 5. Helpers
-- =============================================================================

mapType :: Maybe T.Text -> T.Text -> String
mapType (Just "repeated") t = "[" ++ mapType Nothing t ++ "]"
mapType Nothing t
  | t == "string" = "Text"
  | t == "bytes" = "Text"
  | t == "bool" = "Bool"
  | t == "int32" = "Int"
  | t == "uint32" = "Int"
  | t == "int64" = "Text"
  | t == "uint64" = "Text"
  | t == "double" = "Double"
  | t == "float" = "Float"
  | otherwise = "Maybe " ++ pascal (T.unpack t)
mapType _ _ = error "Unsupported field rule"

writeFileWithDir :: FilePath -> String -> IO ()
writeFileWithDir path content = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path content
