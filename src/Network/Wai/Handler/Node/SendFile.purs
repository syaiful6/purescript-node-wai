module Network.Wai.Handler.Node.SendFile (sendFile) where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)

import Data.ByteString.Node.Stream (Writable, Readable)
import Data.ByteString.Node.File (fdCreateReadableStreamRangeWith, createReadableStreamRangeWith)
import Data.Either (Either(..))
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)

import Network.Wai.Handler.Node.Types (FileId(..), SendFile)
import Node.FS (FS, FileDescriptor)
import Node.FS.Stream (defaultReadStreamOptions, createReadStream, fdCreateReadStream)
import Node.Path (FilePath)

data FileRange = EntireFile | PartOfFile Int Int

sendFile
  :: forall r eff
   . Writable r (fs :: FS | eff)
  -> SendFile (fs :: FS | eff)
sendFile ws (FileId { fileIdPath, fileIdFd }) off len act = case fileIdFd of
  Nothing -> sendFilePath ws fileIdPath (PartOfFile off len) act
  Just fd -> sendFileFd ws fd (PartOfFile off len) act

sendFileFd
  :: forall r eff
   . Writable r (fs :: FS | eff)
  -> FileDescriptor
  -> FileRange
  -> Aff (fs :: FS | eff) Unit
  -> Aff (fs :: FS | eff) Unit
sendFileFd ws fd frange act = do
  rs <- liftEff createStream
  _ <- pipeStreamNoEnd rs ws
  act
  where
  createStream = case frange of
    PartOfFile start end -> fdCreateReadableStreamRangeWith defaultReadStreamOptions start end fd
    EntireFile           -> fdCreateReadStream fd

sendFilePath
  :: forall r eff
   . Writable r (fs :: FS | eff)
  -> FilePath
  -> FileRange
  -> Aff (fs :: FS | eff) Unit
  -> Aff (fs :: FS | eff) Unit
sendFilePath ws path frange act = do
  rs <- liftEff createStream
  _ <- pipeStreamNoEnd rs ws
  act
  where
  createStream = case frange of
    PartOfFile start end -> createReadableStreamRangeWith defaultReadStreamOptions start end path
    EntireFile           -> createReadStream path

pipeStreamNoEnd
  :: forall r w eff
   . Readable w eff
  -> Writable r eff
  -> Aff eff Unit
pipeStreamNoEnd r w = makeAff \cb -> do
  _ <- Fn.runFn5 pipeStreamNoEndEff Left Right r w cb
  pure mempty

foreign import pipeStreamNoEndEff
  :: forall r w eff
   . Fn.Fn5
      (forall x y. x -> Either x y)
      (forall x y. y -> Either x y)
      (Readable w eff)
      (Writable r eff)
      (Either Error Unit -> Eff eff Unit)
      (Eff eff Unit)
