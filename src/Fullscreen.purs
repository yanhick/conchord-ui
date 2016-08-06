module Fullscreen where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import DOM (DOM)

foreign import mkSongFullscreen :: forall eff. Eff (dom :: DOM | eff) Unit
