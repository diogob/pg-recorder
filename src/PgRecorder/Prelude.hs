{-
Welcome to your custom Prelude
Export here everything that should always be in your library scope
For more info on what is exported by Protolude check:
https://github.com/sdiehl/protolude/blob/master/Symbols.md
-}
module PgRecorder.Prelude
    ( module Exports
    , id
    ) where

import Protolude as Exports
import Data.Function (id)
