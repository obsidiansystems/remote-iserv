{-|
Module : GHCi.Remote.Protocol
Description : Remote external interpreter protocol constants
Copyright : (c) 2016, Obsidian Systems LLC
License : BSD3
Maintainer : shea@shealevy.com
Stability : experimental
-}
module GHCi.Remote.Protocol where

import Data.Word

-- | The version of the remote protocol we speak
protocolVersion :: Word8
protocolVersion = 0
