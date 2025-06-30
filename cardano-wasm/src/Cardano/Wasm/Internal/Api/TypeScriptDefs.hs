module Cardano.Wasm.Internal.Api.TypeScriptDefs where

-- import qualified Data.Text.Lazy.IO as TIO

import Data.List.NonEmpty qualified as LNE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

printTypeScriptFile :: TypeScriptFile -> IO ()
printTypeScriptFile tsFile = do
  let content = buildTypeScriptFile tsFile
  putStrLn $ TL.unpack $ TLB.toLazyText content

buildMultilineComment :: Int -> [String] -> TLB.Builder
buildMultilineComment indentLevel commentLines =
  let indentation = TLB.fromLazyText $ TL.replicate (fromIntegral indentLevel) " "
      bodyIndentation = indentation <> TLB.fromString " * "
      firstLine = indentation <> TLB.fromString "/**"
      indentedCommentLines = map (\line -> (bodyIndentation <>) . TLB.fromString $ line <> "\n") commentLines
      lastLine = indentation <> TLB.fromString " */"
   in mconcat [firstLine, "\n", mconcat indentedCommentLines, lastLine]

data TypeScriptFile = TypeScriptFile
  { typeScriptFileName :: String
  , typeScriptFileContent :: [Declaration]
  }

buildTypeScriptFile :: TypeScriptFile -> TLB.Builder
buildTypeScriptFile (TypeScriptFile name decls) =
  let header = TLB.fromString $ "// " ++ name ++ "\n"
      declarations = mconcat $ map (\dec -> "\n" <> buildDeclaration dec <> "\n") decls
   in header <> declarations

data Declaration = Declaration
  { declarationComment :: [String]
  , declarationContent :: DeclarationType
  }

buildDeclaration :: Declaration -> TLB.Builder
buildDeclaration (Declaration [] declarationType) = buildDeclarationType declarationType
buildDeclaration (Declaration comments declarationType) =
  buildMultilineComment 0 comments <> "\n" <> buildDeclarationType declarationType

data DeclarationType
  = ExportDec
      Bool
      -- ^ is default export?
      String
      -- ^ symbol name
  | FunctionDec FunctionHeader
  | InterfaceDec
      String
      -- ^ interface name
      [InterfaceContent]
      -- ^ definitions of the interface

buildDeclarationType :: DeclarationType -> TLB.Builder
buildDeclarationType (ExportDec isDefault symbolName) =
  TLB.fromString "export "
    <> TLB.fromString (if isDefault then "default " else "")
    <> TLB.fromString symbolName
    <> TLB.fromString ";"
buildDeclarationType (FunctionDec header) =
  TLB.fromString "declare function " <> buildFunctionHeader header <> TLB.fromString ";"
buildDeclarationType (InterfaceDec name properties) =
  TLB.fromString "declare interface "
    <> TLB.fromString name
    <> " {"
    <> mconcat (map (\prop -> "\n" <> buildInterfaceContent prop <> "\n") properties)
    <> "}"

data FunctionParam = FunctionParam
  { paramName :: String
  , paramType :: String
  }

buildFunctionParam :: FunctionParam -> TLB.Builder
buildFunctionParam (FunctionParam name pType) =
  TLB.fromString name <> ": " <> TLB.fromString pType

data FunctionHeader = FunctionHeader
  { functionName :: String
  , functionParams :: [FunctionParam]
  , functionReturnType :: String
  }

buildFunctionHeader :: FunctionHeader -> TLB.Builder
buildFunctionHeader (FunctionHeader name params returnType) =
  TLB.fromString name
    <> "("
    <> mconcatWith (TLB.fromString ", ") (map buildFunctionParam params)
    <> "): "
    <> TLB.fromString returnType

data InterfaceContent = InterfaceContent
  { interfaceContentComment :: [String]
  , interfaceContentValue :: InterfaceContentType
  }

buildInterfaceContent :: InterfaceContent -> TLB.Builder
buildInterfaceContent (InterfaceContent [] interfaceType) = buildInterfaceContentType interfaceType
buildInterfaceContent (InterfaceContent comments interfaceType) =
  let indentationAmount = 4
      indentation = TLB.fromLazyText $ TL.replicate (fromIntegral indentationAmount) " "
      comment = buildMultilineComment indentationAmount comments
   in comment <> "\n" <> indentation <> buildInterfaceContentType interfaceType

data InterfaceContentType
  = InterfaceProperty
      String
      -- ^ property name
      String
      -- ^ property type
  | InterfaceMethod
      String
      -- ^ method name
      [FunctionParam]
      -- ^ method parameters
      String
      -- ^ return type

buildInterfaceContentType :: InterfaceContentType -> TLB.Builder
buildInterfaceContentType (InterfaceProperty name pType) =
  TLB.fromString name <> ": " <> TLB.fromString pType <> ";"
buildInterfaceContentType (InterfaceMethod name params returnType) =
  TLB.fromString name
    <> "("
    <> mconcatWith (TLB.fromString ", ") (map buildFunctionParam params)
    <> "): "
    <> TLB.fromString returnType
    <> ";"

mconcatWith :: TLB.Builder -> [TLB.Builder] -> TLB.Builder
mconcatWith separator = maybe mempty (foldr1 (\a b -> a <> separator <> b)) . LNE.nonEmpty
