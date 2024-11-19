{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hedgehog.Golden.ErrorMessage where

import           Cardano.Api (Error (..))
import           Cardano.Api.Pretty

import qualified Control.Concurrent.QSem as IO
import           Control.Exception (bracket_)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Algorithm.Diff (PolyDiff (Both), getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)
import           Data.Data
import qualified Data.List as List
import           GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified GHC.Stack as GHC
import qualified System.Directory as IO
import qualified System.Environment as IO
import           System.FilePath (takeDirectory, (</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as IO

import           Hedgehog
import qualified Hedgehog.Extras.Test as H
import qualified Hedgehog.Internal.Property as H
import           Test.Tasty
import           Test.Tasty.Hedgehog

-- | Generate test tree for the list of values. This 'TestTree' will serialize the values using 'Error'
-- instance and compare them against golden files in the provided location.
testAllErrorMessages
  :: forall a
   . (HasCallStack, Data a, Error a)
  => FilePath
  -- ^ golden files location
  -> [a]
  -- ^ list of values to test against
  -> TestTree
testAllErrorMessages goldenFilesLocation errs = withFrozenCallStack $ do
  -- 'err' here is only needed for its 'Data' instance and it's never evaluated
  -- it's equivalent of having @err = undefined :: a@
  let err = undefined :: a
      typeName = show $ typeOf err
      testedConstructors = map toConstr errs
      allConstructors = dataTypeConstrs $ dataTypeOf err
      notTestedConstructors = [c | c <- allConstructors, c `notElem` testedConstructors]
      testAllConstructors =
        testProperty "check if all constructors are tested" . withTests 1 . property $ do
          H.note_ $ "Untested constructors: " <> show notTestedConstructors
          notTestedConstructors === []

  testGroup typeName $
    testAllConstructors : map (testErrorMessage goldenFilesLocation) errs

-- | Creates error messages for all values and tests them against the golden files.
--
-- An escape hatch when adding of 'Data a' instance gets impossible (like when we embed 'TypeRep' in our error
-- data types) or requires significant multi-package changes and outweighs the benefits here.
testAllErrorMessages_
  :: forall a
   . (HasCallStack, Error a)
  => FilePath
  -- ^ golden files path
  -> String
  -- ^ module name
  -> String
  -- ^ type name
  -> [(String, a)]
  -- ^ list of constructor names and values
  -> TestTree
testAllErrorMessages_ goldenFilesLocation moduleName typeName errs = withFrozenCallStack $ do
  testGroup typeName $
    fmap (uncurry $ testErrorMessage_ goldenFilesLocation moduleName typeName) errs

-- | Create 'TestTree' validating serialized value @a@ using 'Error' against the golden files.
testErrorMessage
  :: (HasCallStack, Data a, Error a)
  => FilePath
  -- ^ golden files path
  -> a
  -- ^ value to test
  -> TestTree
testErrorMessage goldenFilesLocation err = withFrozenCallStack $ do
  let errTypeRep = typeOf err
      typeName = show errTypeRep
      moduleName = tyConModule $ typeRepTyCon errTypeRep
      constructorName = show $ toConstr err
  testErrorMessage_ goldenFilesLocation moduleName typeName constructorName err

-- | Create 'TestTree' validating serialized value @a@ using 'Error' against the golden files.
--
-- Requires providing a module name, a type name and a constructor name of @a@. Useful when 'Data a'
-- instance is not available.
testErrorMessage_
  :: (HasCallStack, Error a)
  => FilePath
  -- ^ golden files path
  -> String
  -- ^ module name
  -> String
  -- ^ type name
  -> String
  -- ^ constructor name
  -> a
  -- ^ value to test
  -> TestTree
testErrorMessage_ goldenFilesLocation moduleName typeName constructorName err = withFrozenCallStack $ do
  let fqtn = moduleName <> "." <> typeName
  testProperty constructorName . withTests 1 . property $ do
    H.note_ "Incorrect error message in golden file"
    H.note_ "What the value looks like in memory"
    let pErr = docToString (prettyError err)
    H.note_ $ show pErr
    diffVsGoldenFile
      pErr
      (goldenFilesLocation </> fqtn </> constructorName <> ".txt")

-- Upstream to hedgehog-extras
diffVsGoldenFile
  :: HasCallStack
  => (MonadIO m, MonadTest m)
  => String
  -- ^ Actual content
  -> FilePath
  -- ^ Reference file
  -> m ()
diffVsGoldenFile actualContent goldenFile = GHC.withFrozenCallStack $ do
  forM_ mGoldenFileLogFile $ \logFile ->
    liftIO $ semBracket $ IO.appendFile logFile $ goldenFile <> "\n"

  fileExists <- liftIO $ IO.doesFileExist goldenFile

  if
    | recreateGoldenFiles -> writeGoldenFile goldenFile actualContent
    | fileExists          -> checkAgainstGoldenFile goldenFile actualLines
    | createGoldenFiles   -> writeGoldenFile goldenFile actualContent
    | otherwise           -> reportGoldenFileMissing goldenFile
 where
  actualLines = List.lines actualContent

writeGoldenFile
  :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> String
  -> m ()
writeGoldenFile goldenFile actualContent = GHC.withFrozenCallStack $ do
  H.note_ $ "Creating golden file " <> goldenFile
  H.createDirectoryIfMissing_ (takeDirectory goldenFile)
  writeFile' goldenFile actualContent

recreateGoldenFiles :: Bool
recreateGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "RECREATE_GOLDEN_FILES"
  return $ value == Just "1"

createGoldenFiles :: Bool
createGoldenFiles = IO.unsafePerformIO $ do
  value <- IO.lookupEnv "CREATE_GOLDEN_FILES"
  return $ value == Just "1"

writeFile' :: (MonadTest m, MonadIO m, HasCallStack) => FilePath -> String -> m ()
writeFile' filePath contents = GHC.withFrozenCallStack $ do
  void . H.annotate $ "Writing file: " <> filePath
  H.evalIO $ IO.withFile filePath IO.WriteMode $ \handle -> do
    IO.hSetEncoding handle IO.utf8
    IO.hPutStr handle contents

checkAgainstGoldenFile
  :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> [String]
  -> m ()
checkAgainstGoldenFile goldenFile actualLines = GHC.withFrozenCallStack $ do
  referenceLines <- liftIO $ IO.withFile goldenFile IO.ReadMode $ \handle -> do
    IO.hSetEncoding handle IO.utf8
    List.lines <$> IO.hGetContents' handle
  let difference = getGroupedDiff actualLines referenceLines
  case difference of
    [] -> pure ()
    [Both{}] -> pure ()
    _ -> do
      H.note_ $
        unlines
          [ "Golden test failed against the golden file."
          , "To recreate golden file, run with RECREATE_GOLDEN_FILES=1."
          ]
      H.failMessage GHC.callStack $ ppDiff difference

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

semBracket :: IO a -> IO a
semBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

mGoldenFileLogFile :: Maybe FilePath
mGoldenFileLogFile =
  IO.unsafePerformIO $
    IO.lookupEnv "GOLDEN_FILE_LOG_FILE"

reportGoldenFileMissing
  :: ()
  => HasCallStack
  => MonadIO m
  => MonadTest m
  => FilePath
  -> m ()
reportGoldenFileMissing goldenFile = GHC.withFrozenCallStack $ do
  H.note_ $
    unlines
      [ "Golden file " <> goldenFile <> " does not exist."
      , "To create it, run with CREATE_GOLDEN_FILES=1."
      , "To recreate it, run with RECREATE_GOLDEN_FILES=1."
      ]
  H.failure
