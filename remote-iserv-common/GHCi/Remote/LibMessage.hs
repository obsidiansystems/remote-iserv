{-# LANGUAGE DeriveGeneric #-}
{-|
Module : GHCi.Remote.LibMessage
Description : Message type for sending libraries to a remote external interpreter
Copyright : (c) 2016, Obsidian Systems LLC
License : BSD3
Maintainer : shea@shealevy.com
Stability : experimental
-}
module GHCi.Remote.LibMessage where

import Data.Binary
import Data.ByteString (ByteString)
import GHC.Generics

-- | A message in the library sending protocol
data LibMessage
  -- | A non-terminal chunk of the file
  = Chunk !ByteString
  -- | The last chunk of the file
  | Done !ByteString deriving (Generic)

-- | Serialize a LibMessage
instance Binary LibMessage
