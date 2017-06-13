module Network.Wai.Handler.Node.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)

import Node.FS (FS)
import Node.HTTP (HTTP)


type WaiEffects eff =
  ( avar :: AVAR
  , exception :: EXCEPTION
  , fs :: FS
  , http :: HTTP
  , ref :: REF
  | eff
  )
