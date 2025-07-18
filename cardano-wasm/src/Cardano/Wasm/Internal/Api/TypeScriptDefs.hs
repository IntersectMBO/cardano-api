{-# LANGUAGE OverloadedStrings #-}

-- | This module defines a basic AST for generating TypeScript
-- declaration files, and basic pretty-printing functionality.
--
-- The reason we define a custom tool for generating TypeScript
-- declaration files is that existing libraries like `aeson-typescript`
-- and `servant-typescript` are not aimed at generating custom
-- TypeScript interface declaration files for a specific API,
-- but rather at generating TypeScript interfaces for Haskell
-- data types and servant HTTP APIs respectively. And other libraries
-- that align with our needs, like `language-typescript`, are not
-- actively maintained.
module Cardano.Wasm.Internal.Api.TypeScriptDefs where

import Data.List.NonEmpty qualified as LNE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB

-- | Prints the TypeScript declaration file to stdout.
printTypeScriptFile :: TypeScriptFile -> IO ()
printTypeScriptFile tsFile = do
  let content = buildTypeScriptFile tsFile
  putStrLn $ TL.unpack $ TLB.toLazyText content

-- | Creates a builder for a JavaScript-style multiline comment
-- with the specified indentation level (in spaces) and each line
-- in the list of strings as a separate line in the comment.
-- The first line starts with `/**`, subsequent lines (corresponding
-- to each line in the list) start with ` * `,
-- and the last line ends with ` */`.
-- The indentation level is used to indent the entire comment block.
buildMultilineComment :: Int -> [String] -> TLB.Builder
buildMultilineComment indentLevel commentLines =
  let indentation = TLB.fromLazyText $ TL.replicate (fromIntegral indentLevel) " "
      bodyIndentation = indentation <> " * "
      firstLine = indentation <> "/**"
      indentedCommentLines = map (\line -> (bodyIndentation <>) . TLB.fromString $ line <> "\n") commentLines
      lastLine = indentation <> " */"
   in mconcat [firstLine, "\n", mconcat indentedCommentLines, lastLine]

-- | Represents the top-level structure of a TypeScript declaration file.
data TypeScriptFile = TypeScriptFile
  { typeScriptFileName :: String
  -- ^ Name of the TypeScript file.
  , typeScriptFileContent :: [Declaration]
  -- ^ List of declarations in the file.
  }

-- | Creates a builder for a TypeScript declaration file.
-- It adds a comment to the top of the file with the file name.
buildTypeScriptFile :: TypeScriptFile -> TLB.Builder
buildTypeScriptFile (TypeScriptFile name decls) =
  let header = TLB.fromString $ "// " ++ name ++ "\n"
      declarations = mconcat $ map (\dec -> "\n" <> buildDeclaration dec <> "\n") decls
   in header <> declarations

-- | Wraps a TypeScript declaration with a comment.
-- The TypeScript declaration can have any of the types
-- defined by 'DeclarationType'.
data Declaration = Declaration
  { declarationComment :: [String]
  -- ^ Comments for the declaration, can be empty if no comments are needed.
  -- Each element in the list is a separate line in the comment.
  , declarationContent :: DeclarationType
  -- ^ The type and content of the declaration.
  }

-- | Creates a builder for a TypeScript declaration.
buildDeclaration :: Declaration -> TLB.Builder
buildDeclaration (Declaration [] declarationType) = buildDeclarationType declarationType
buildDeclaration (Declaration comments declarationType) =
  buildMultilineComment 0 comments <> "\n" <> buildDeclarationType declarationType

-- | Represents a TypeScript declaration content of some type.
data DeclarationType
  = -- | Export declaration.
    ExportDec
      Bool
      -- ^ Is it a default export?
      String
      -- ^ Name of the symbol to export.
  | -- | Function declaration.
    FunctionDec FunctionHeader
  | -- | Interface declaration.
    InterfaceDec
      String
      -- ^ Name of the interface.
      [InterfaceContent]
      -- ^ Definitions of the interface.

-- | Creates a builder for a TypeScript declaration type and content.
buildDeclarationType :: DeclarationType -> TLB.Builder
buildDeclarationType (ExportDec isDefault symbolName) =
  "export "
    <> (if isDefault then "default " else "")
    <> TLB.fromString symbolName
    <> ";"
buildDeclarationType (FunctionDec header) =
  "declare function " <> buildFunctionHeader header <> ";"
buildDeclarationType (InterfaceDec name properties) =
  "declare interface "
    <> TLB.fromString name
    <> " {"
    <> mconcat (map (\prop -> "\n" <> buildInterfaceContent prop <> "\n") properties)
    <> "}"

-- | Represents a function parameter in TypeScript.
data FunctionParam = FunctionParam
  { paramName :: String
  -- ^ Name of the parameter.
  , paramType :: String
  -- ^ Type of the parameter.
  }

-- | Creates a builder for a TypeScript function parameter.
buildFunctionParam :: FunctionParam -> TLB.Builder
buildFunctionParam (FunctionParam name pType) =
  TLB.fromString name <> ": " <> TLB.fromString pType

-- | Represents a TypeScript function header.
data FunctionHeader = FunctionHeader
  { functionName :: String
  -- ^ Name of the function.
  , functionParams :: [FunctionParam]
  -- ^ List of parameters of the function.
  , functionReturnType :: String
  -- ^ Return type of the function.
  }

-- | Creates a builder for a TypeScript function header.
buildFunctionHeader :: FunctionHeader -> TLB.Builder
buildFunctionHeader (FunctionHeader name params returnType) =
  TLB.fromString name
    <> "("
    <> mconcatWith ", " (map buildFunctionParam params)
    <> "): "
    <> TLB.fromString returnType

-- | Represents a TypeScript interface content of some type
-- out of the ones defined by 'InterfaceContentType'.
data InterfaceContent = InterfaceContent
  { interfaceContentComment :: [String]
  -- ^ Comments for the interface content.
  , interfaceContentValue :: InterfaceContentType
  -- ^ The type and content of the interface.
  }

-- | Creates a builder for a TypeScript interface content.
buildInterfaceContent :: InterfaceContent -> TLB.Builder
buildInterfaceContent (InterfaceContent [] interfaceType) = buildInterfaceContentType interfaceType
buildInterfaceContent (InterfaceContent comments interfaceType) =
  let indentationAmount = 4
      indentation = TLB.fromLazyText $ TL.replicate (fromIntegral indentationAmount) " "
      comment = buildMultilineComment indentationAmount comments
   in comment <> "\n" <> indentation <> buildInterfaceContentType interfaceType

-- | Represents a TypeScript interface type and content.
data InterfaceContentType
  = -- | Defines a property in the interface.
    InterfaceProperty
      String
      -- ^ Property name.
      String
      -- ^ Property type.
  | -- | Defines a method in the interface.
    InterfaceMethod
      String
      -- ^ Method name.
      [FunctionParam]
      -- ^ Method parameters.
      String
      -- ^ Return type of the method.

-- | Creates a builder for a TypeScript interface content and type.
buildInterfaceContentType :: InterfaceContentType -> TLB.Builder
buildInterfaceContentType (InterfaceProperty name pType) =
  TLB.fromString name <> ": " <> TLB.fromString pType <> ";"
buildInterfaceContentType (InterfaceMethod name params returnType) =
  TLB.fromString name
    <> "("
    <> mconcatWith ", " (map buildFunctionParam params)
    <> "): "
    <> TLB.fromString returnType
    <> ";"

-- | Concatenates a list of builders with a separator.
-- If the list is empty, it returns an empty builder.
mconcatWith :: TLB.Builder -> [TLB.Builder] -> TLB.Builder
mconcatWith separator = maybe mempty (foldr1 (\a b -> a <> separator <> b)) . LNE.nonEmpty
