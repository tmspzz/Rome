{-# LANGUAGE OverloadedStrings #-}

module Engine.Uploading where

import qualified Codec.Archive.Zip            as Zip
import           Control.Monad                (when)
import           Control.Monad.Reader         (ReaderT, ask, withReaderT)
import qualified Data.ByteString.Lazy         as LBS
import           Data.Carthage.TargetPlatform
import           Data.Monoid                  ((<>))
import           Data.Romefile                (Framework (..))
import qualified Data.Text                    as T
import qualified Network.AWS                  as AWS
import qualified Network.AWS.S3               as S3
import           System.FilePath              ((</>))
import           Types                        hiding (version)
import           Utils
import           Xcode.DWARF



-- | Uploads a Framework `Zip.Archive` to an engine.
uploadFrameworkToEngine
  :: Zip.Archive -- ^ The `Zip.Archive` of the Framework.
  -> FilePath -- ^ The engine definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework.
  -> TargetPlatform -- ^ A `TargetPlatform`s restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadFrameworkToEngine frameworkArchive s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = undefined



-- | Uploads a dSYM `Zip.Archive` to an engine.
uploadDsymToEngine
  :: Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> FilePath -- ^ The engine definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadDsymToEngine dSYMArchive s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = undefined



-- | Uploads a bcsymbolmap `Zip.Archive` to an engine.
uploadBcsymbolmapToEngine
  :: DwarfUUID -- ^ The UUID of the bcsymbolmap
  -> Zip.Archive -- ^ The `Zip.Archive` of the dSYM.
  -> FilePath -- ^ The engine definition.
  -> InvertedRepositoryMap -- ^ The map used to resolve `FrameworkName`s to `GitRepoName`s.
  -> FrameworkVersion -- ^ The `FrameworkVersion` identifying the Framework and the dSYM.
  -> TargetPlatform -- ^ A `TargetPlatform` restricting the scope of this action.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadBcsymbolmapToEngine dwarfUUID dwarfArchive s3BucketName reverseRomeMap (FrameworkVersion f@(Framework fwn _ fwps) version) platform
  = undefined



-- | Uploads a .version file to an engine
uploadVersionFileToEngine'
  :: FilePath -- ^ The engine definition.
  -> LBS.ByteString -- ^ The contents of the .version file.
  -> ProjectNameAndVersion -- ^ The information used to derive the name and path for the .version file.
  -> ReaderT (CachePrefix, Bool) IO ()
uploadVersionFileToEngine' s3BucketName versionFileContent projectNameAndVersion = undefined



-- | Uploads an artifact to an engine
uploadBinary
  :: AWS.ToBody a
  => S3.BucketName
  -> a
  -> FilePath
  -> FilePath
  -> ReaderT (Bool) IO ()
uploadBinary s3BucketName binaryZip destinationPath objectName = undefined

