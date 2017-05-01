module Network.HTTP.Wai.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)

import Node.FS (FS)
import Node.HTTP (HTTP)
import Node.Buffer (BUFFER)

type WaiEffects eff =
  ( avar :: AVAR
  , buffer :: BUFFER
  , exception :: EXCEPTION
  , fs :: FS
  , http :: HTTP
  , ref :: REF
  | eff
  )
