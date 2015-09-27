module IndexHandler (handleIndex)
where

import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist

import           Application

handleIndex :: Handler App App ()
handleIndex = render "app"
