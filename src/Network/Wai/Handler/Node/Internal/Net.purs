module Network.Wai.Handler.Node.Internal.Net
  ( Server
  , Socket
  , createServer
  , listenSocket
  , ListenOptions
  , listen
  , socketAsStrem
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)

import Unsafe.Coerce (unsafeCoerce)

import Node.Stream (Duplex)

-- | The type of net Server
foreign import data Server :: Type

-- | The type of net Socket
foreign import data Socket :: Type

foreign import createServer :: forall eff. (Socket -> Eff eff Unit) -> Eff eff Server

-- | Start a IPC server
foreign import listenSocket
  :: forall eff
   . Server
  -> String
  -> Eff eff Unit
  -> Eff eff Unit

listen :: forall eff. Server -> ListenOptions -> Eff eff Unit -> Eff eff Unit
listen server opts done = listenImpl server (opts { backlog = toNullable opts.backlog }) done

socketAsStrem :: forall eff. Socket -> Duplex eff
socketAsStrem = unsafeCoerce

destroy :: forall eff. Maybe Error -> Socket -> Eff eff Unit
destroy me sock = destroyImpl (toNullable me) sock

type ListenOptions =
  { hostname :: String
  , port     :: Int
  , backlog  :: Maybe Int
  }

type ListenOptions' =
  { hostname :: String
  , port     :: Int
  , backlog  :: Nullable Int
  }

foreign import listenImpl :: forall eff. Server -> ListenOptions' -> Eff eff Unit -> Eff eff Unit

foreign import destroyImpl :: forall eff. Nullable Error -> Socket -> Eff eff Unit
